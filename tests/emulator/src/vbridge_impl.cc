#include <fmt/core.h>
#include <glog/logging.h>

#include <disasm.h>

#ifdef COSIM_VERILATOR
#include <verilated.h>
#endif

#include "exceptions.h"
#include "glog_exception_safe.h"
#include "util.h"
#include "vbridge_impl.h"

/// convert TL style size to size_by_bytes
inline uint32_t decode_size(uint32_t encoded_size) {
  return 1 << encoded_size;
}

void VBridgeImpl::timeoutCheck() {
  if (get_t() > timeout + last_commit_time) {
    LOG(FATAL_S) << fmt::format("Simulation timeout, t={}, last_commit={}", get_t(), last_commit_time);
  }
}

void VBridgeImpl::dpiInitCosim() {
  google::InitGoogleLogging("emulator");

#ifdef COSIM_VERILATOR
  ctx = Verilated::threadContextp();
#endif
  LOG(INFO) << fmt::format("[{}] dpiInitCosim", getCycle());
  proc.reset();
  // TODO: remove this line, and use CSR write in the test code to enable this the VS field.
  proc.get_state()->sstatus->write(proc.get_state()->sstatus->read() | SSTATUS_VS);
  sim.load(bin, reset_vector);
  init_spike();
  LOG(INFO) << fmt::format("Simulation Environment Initialized: bin={}, wave={}, reset_vector={:#x}, timeout={}",
                           bin, wave, reset_vector, timeout);
  dpiDumpWave();
}

/* cosim                          rtl
 * clock = 1
 * (prepare spike_event)
 *    <----- lsu idx -----------  (peekLsuEnq      posedge 1)
 *    <---- rf access ----------  (peekVrfWrite    posedge 1)
 *    <------- resp ------------  (peekResponse    posedge 1)
 *    <------- inst,csr --------  (peekInstruction posedge 1)
 *
 *    ------- inst ------------>  (peekIssue       posedge 2)
 *    ------- tl resp --------->  (pokeTL          posedge 2)
 *
 *    <------ tl req -----------  (peekTL          posedge 3)
 * clock = 0, eval
 *    <---- rf queue access ----  (peekWriteQueue  negedge 1)
 */

//==================
// posedge (1)
//==================

void VBridgeImpl::dpiPeekLsuEnq(const VLsuReqEnqPeek &lsu_req_enq) {
  VLOG(3) << fmt::format("[{}] dpiPeekLsuEnq", get_t());
  update_lsu_idx(lsu_req_enq);
}

void VBridgeImpl::dpiPeekVrfWrite(const VrfWritePeek &vrf_write) {
  VLOG(3) << fmt::format("[{}] dpiPeekVrfWrite", get_t());
  CHECK_S(0 <= vrf_write.lane_index && vrf_write.lane_index < consts::numLanes);
  record_rf_accesses(vrf_write);
}

void VBridgeImpl::dpiPokeInst(const VInstrInterfacePoke &v_instr, const VCsrInterfacePoke &v_csr, const VRespInterface &v_resp) {
  VLOG(3) << fmt::format("[{}] dpiPokeInst", get_t());
  for (auto &se:to_rtl_queue) {
    VLOG(2) << fmt::format(" - se={}, issued={}, issue_idx={}", se.describe_insn(), se.is_issued, se.issue_idx);
  }

  if (v_resp.valid) {
    LOG(INFO) << fmt::format("[{}] prepare to commit", get_t());
    SpikeEvent &se = to_rtl_queue.back();
    se.record_rd_write(v_resp);
    se.check_is_ready_for_commit();
    LOG(INFO) << fmt::format("[{}] rtl commit insn ({})", get_t(), to_rtl_queue.back().describe_insn());
    last_commit_time = get_t();
    to_rtl_queue.pop_back();
  }

  while (true) {
    se_to_issue = find_se_to_issue();
    if ((se_to_issue->is_vfence_insn || se_to_issue->is_exit_insn) && to_rtl_queue.size() == 1) {
      if (se_to_issue->is_exit_insn) {
        LOG(INFO) << fmt::format("[{}] reaching exit instruction ({})", get_t(), se_to_issue->describe_insn());
        throw ReturnException();
      }

      to_rtl_queue.pop_back();
    } else {
      break;
    }
  }

  if (se_to_issue->is_vfence_insn || se_to_issue->is_exit_insn) {
    // it is ensured there are some other instruction not committed, thus se_to_issue should not be issued
    CHECK_S(to_rtl_queue.size() > 1);
    if (se_to_issue->is_exit_insn) {
      LOG(INFO) << fmt::format("[{}] exit waiting for fence", get_t());
    } else {
      LOG(INFO) << fmt::format("[{}] waiting for fence, no issuing new instruction", get_t());
    }
    *v_instr.valid = false;
  } else {
    LOG(INFO) << fmt::format("[{}] poke instruction ({})",
                             get_t(), se_to_issue->describe_insn());
    se_to_issue->drive_rtl_req(v_instr);
  }
  se_to_issue->drive_rtl_csr(v_csr);
}

//==================
// posedge (2)
//==================

void VBridgeImpl::dpiPeekIssue(svBit ready, svBitVecVal issueIdx) {
  VLOG(3) << fmt::format("[{}] {}", get_t(), __func__);
  if (ready && !(se_to_issue->is_vfence_insn || se_to_issue->is_exit_insn)) {
    se_to_issue->is_issued = true;
    se_to_issue->issue_idx = issueIdx;
    LOG(INFO) << fmt::format("[{}] issue to rtl ({}, issue_idx={})", get_t(), se_to_issue->describe_insn(), issueIdx);
  }
}

void VBridgeImpl::dpiPokeTL(const VTlInterfacePoke &v_tl_resp) {
  VLOG(3) << fmt::format("[{}] dpiPokeTL", get_t());
  CHECK_S(0 <= v_tl_resp.channel_id && v_tl_resp.channel_id < consts::numTL);
  return_tl_response(v_tl_resp);
}

//==================
// posedge (3)
//==================

void VBridgeImpl::dpiPeekTL(const VTlInterface &v_tl) {
  VLOG(3) << fmt::format("[{}] dpiPeekTL", get_t());
  CHECK_S(0 <= v_tl.channel_id && v_tl.channel_id < consts::numTL);
  receive_tl_req(v_tl);
}

//==================
// negedge (1)
//==================

void VBridgeImpl::dpiPeekWriteQueue(const VLsuWriteQueuePeek &lsu_queue) {
  VLOG(3) << fmt::format("[{}] dpiPeekWriteQueue", get_t());
  CHECK_S(0 <= lsu_queue.mshr_index && lsu_queue.mshr_index < consts::numMSHR);
  record_rf_queue_accesses(lsu_queue);

  se_to_issue = nullptr;   // clear se_to_issue, to avoid using the wrong one
}

VBridgeImpl::VBridgeImpl() :
    sim(1 << 30),
    isa("rv32gcv", "M"),
    proc(
        /*isa*/ &isa,
        /*varch*/ fmt::format("vlen:{},elen:{}", consts::vlen_in_bits, consts::elen).c_str(),
        /*sim*/ &sim,
        /*id*/ 0,
        /*halt on reset*/ true,
        /* endianness*/ memif_endianness_little,
        /*log_file_t*/ nullptr,
        /*sout*/ std::cerr),
    vrf_shadow(std::make_unique<uint8_t[]>(consts::vlen_in_bytes * consts::numVRF)){

  auto& csrmap = proc.get_state()->csrmap;
  csrmap[CSR_MSIMEND] = std::make_shared<basic_csr_t>(&proc, CSR_MSIMEND, 0);
}

void VBridgeImpl::init_spike() {
  // reset spike CPU
  proc.reset();
  // load binary to reset_vector
  sim.load(bin, reset_vector);
}

uint64_t VBridgeImpl::get_t() {
  return getCycle();
}

std::optional<SpikeEvent> VBridgeImpl::spike_step() {
  auto state = proc.get_state();
  auto fetch = proc.get_mmu()->load_insn(state->pc);
  auto event = create_spike_event(fetch);
  auto &xr = proc.get_state()->XPR;

  reg_t pc;
  clear_state(proc);
  if (event) {
    auto &se = event.value();
    LOG(INFO) << fmt::format("spike start exec insn ({}) (vl={}, sew={}, lmul={})",
                             se.describe_insn(), se.vl, (int) se.vsew, (int) se.vlmul);
    se.pre_log_arch_changes();
    pc = fetch.func(&proc, fetch.insn, state->pc);
    se.log_arch_changes();
  } else {
    pc = fetch.func(&proc, fetch.insn, state->pc);
  }

  // Bypass CSR insns commitlog stuff.
  if (!invalid_pc(pc)) {
    state->pc = pc;
  } else if (pc == PC_SERIALIZE_BEFORE) {
    // CSRs are in a well-defined state.
    state->serialized = true;
  }

  return event;
}

std::optional<SpikeEvent> VBridgeImpl::create_spike_event(insn_fetch_t fetch) {
  // create SpikeEvent
  uint32_t opcode = clip(fetch.insn.bits(), 0, 6);
  uint32_t width = clip(fetch.insn.bits(), 12, 14);
  uint32_t rs1 = clip(fetch.insn.bits(), 15, 19);
  uint32_t csr = clip(fetch.insn.bits(), 20, 31);

  // for load/store instr, the opcode is shared with fp load/store. They can be only distinguished by func3 (i.e. width)
  // the func3 values for vector load/store are 000, 101, 110, 111, we can filter them out by ((width - 1) & 0b100)
  bool is_load_type  = opcode == 0b0000111 && ((width - 1) & 0b100);
  bool is_store_type = opcode == 0b0100111 && ((width - 1) & 0b100);
  bool is_v_type = opcode == 0b1010111;

  bool is_csr_type = opcode == 0b1110011 && (width & 0b011);
  bool is_csr_write = is_csr_type && ((width & 0b100) | rs1);

  if (is_load_type || is_store_type || is_v_type || (
      is_csr_write && csr == CSR_MSIMEND)) {
    return SpikeEvent{proc, fetch, this};
  } else {
    return {};
  }
}

uint8_t VBridgeImpl::load(uint64_t address){
  return *sim.addr_to_mem(address);
}

void VBridgeImpl::receive_tl_req(const VTlInterface &tl) {
  // TODO: remove this macro since it is unnecessary
  int tlIdx = tl.channel_id;
  if (!tl.a_valid) return;

  uint8_t opcode = tl.a_bits_opcode;
  uint32_t addr = tl.a_bits_address;
  uint8_t size = tl.a_bits_size;
  uint16_t src = tl.a_bits_source;   // MSHR id, TODO: be returned in D channel
  uint32_t lsu_index = tl.a_bits_source & 3;
  SpikeEvent *se;
  for (auto se_iter = to_rtl_queue.rbegin(); se_iter != to_rtl_queue.rend(); se_iter++) {
    if (se_iter->lsu_idx == lsu_index) {
      se = &(*se_iter);
    }
  }
  CHECK_S(se) << fmt::format(": [{]] cannot find SpikeEvent with lsu_idx={}", get_t(), lsu_index);

  switch (opcode) {

  case TlOpcode::Get: {
    auto mem_read = se->mem_access_record.all_reads.find(addr);
    CHECK_S(mem_read != se->mem_access_record.all_reads.end())
      << fmt::format(": [{}] cannot find mem read of addr {:08X}", get_t(), addr);
    CHECK_EQ_S(mem_read->second.size_by_byte, decode_size(size)) << fmt::format(
        ": [{}] expect mem read of size {}, actual size {} (addr={:08X}, {})",
        get_t(), mem_read->second.size_by_byte, 1 << decode_size(size), addr, se->describe_insn());

    uint64_t data = mem_read->second.val;
    LOG(INFO) << fmt::format("[{}] receive rtl mem get req (addr={:08X}, size={}byte, src={:04X}), should return data {:04X}",
                             get_t(), addr, decode_size(size), src, data);
    tl_banks[tlIdx].emplace(std::make_pair(addr, TLReqRecord{
        data, 1u << size, src, TLReqRecord::opType::Get, get_mem_req_cycles()
    }));
    mem_read->second.executed = true;
    break;
  }

  case TlOpcode::PutFullData: {
    uint32_t data = tl.a_bits_data;
    int offset_by_bits = addr % 4 * 8; // TODO: replace 4 with XLEN.
    data = clip(data, offset_by_bits, offset_by_bits + decode_size(size)*8 - 1);
    LOG(INFO) << fmt::format("[{}] receive rtl mem put req (addr={:08X}, size={}byte, src={:04X}, data={})",
                             get_t(), addr, decode_size(size), src, data);
    auto mem_write = se->mem_access_record.all_writes.find(addr);

    CHECK_S(mem_write != se->mem_access_record.all_writes.end())
            << fmt::format(": [{}] cannot find mem write of addr={:08X}", get_t(), addr);
    CHECK_EQ_S(mem_write->second.size_by_byte, decode_size(size)) << fmt::format(
        ": [{}] expect mem write of size {}, actual size {} (addr={:08X}, insn='{}')",
        get_t(), mem_write->second.size_by_byte, 1 << decode_size(size), addr, se->describe_insn());
    CHECK_EQ_S(mem_write->second.val, data) << fmt::format(
        ": [{}] expect mem write of data {:08X}, actual data {:08X} (addr={:08X}, insn='{}')",
        get_t(), mem_write->second.val, data, addr, se->describe_insn());

    tl_banks[tlIdx].emplace(std::make_pair(addr, TLReqRecord{
        data, 1u << size, src, TLReqRecord::opType::PutFullData, get_mem_req_cycles()
    }));
    mem_write->second.executed = true;
    break;
  }
  default: {
    LOG(FATAL_S) << fmt::format("unknown tl opcode {}", opcode);
  }
  }
}

void VBridgeImpl::return_tl_response(const VTlInterfacePoke &tl_poke) {
  // update remaining_cycles
  auto i = tl_poke.channel_id;
  for (auto &[addr, record]: tl_banks[i]) {
    if (record.remaining_cycles > 0) record.remaining_cycles--;
  }

  // find a finished request and return
  bool d_valid = false;
  *tl_poke.d_bits_source = 0;   // just for cleanness of the waveform, no effect
  for (auto &[addr, record]: tl_banks[i]) {

    if (record.remaining_cycles == 0) {
      LOG(INFO) << fmt::format("[{}] return rtl mem get resp (addr={:08X}, size={}byte, src={:04X}, data={:08X}), ready={}",
                             get_t(), addr, record.size_by_byte, record.source, record.data, tl_poke.d_ready);
      *tl_poke.d_bits_opcode = record.op == TLReqRecord::opType::Get ? TlOpcode::AccessAckData : TlOpcode::AccessAck;
      *tl_poke.d_bits_data = record.data;
      *tl_poke.d_bits_source = record.source;
      *tl_poke.d_bits_sink = 0;
      *tl_poke.d_corrupt = false;
      *tl_poke.d_bits_denied = false;
      d_valid = true;
      if (tl_poke.d_ready) {
        record.op = TLReqRecord::opType::Nil;
      }
      break;
    }
  }
  *tl_poke.d_valid = d_valid;

  // collect garbage
  erase_if(tl_banks[i], [](const auto &record) {
    return record.second.op == TLReqRecord::opType::Nil;
  });

  // welcome new requests all the time
  *tl_poke.a_ready = true;
}

void VBridgeImpl::update_lsu_idx(const VLsuReqEnqPeek &enq) {
  uint32_t lsuReqs[consts::numMSHR];
  for (int i = 0; i < consts::numMSHR; i++) {
    lsuReqs[i] = (enq.enq >> i) & 1;
  }
  for (auto se = to_rtl_queue.rbegin(); se != to_rtl_queue.rend(); se++) {
    if (se->is_issued && (se->is_load || se->is_store) && (se->lsu_idx == consts::lsuIdxDefault)) {
      uint8_t index = consts::lsuIdxDefault;
      for (int i = 0; i < consts::numMSHR; i++) {
        if (lsuReqs[i] == 1) {
          index = i;
          break;
        }
      }
      CHECK_NE_S(index, consts::lsuIdxDefault)
        << fmt::format(": [{}] load store issued but not no slot allocated.", get_t());
      se->lsu_idx = index;
      LOG(INFO) << fmt::format("[{}] insn ({}) is allocated lsu_idx={}", get_t(), se->describe_insn(), index);
      break;
    }
  }
}

SpikeEvent *VBridgeImpl::find_se_to_issue() {
  SpikeEvent *unissued_se = nullptr;

  // search from tail, until finding an unissued se
  for (auto iter = to_rtl_queue.rbegin(); iter != to_rtl_queue.rend(); iter++) {
    if (!iter->is_issued) {
      unissued_se = &(*iter);
      break;
    }
  }

  // if no se is found, step spike to produce an se
  try {
    while (unissued_se == nullptr) {
      if (auto spike_event = spike_step()) {
        SpikeEvent &se = spike_event.value();
        to_rtl_queue.push_front(std::move(se));  // se cannot be copied since it has reference member
        unissued_se = &to_rtl_queue.front();
      }
    }
    return unissued_se;
  } catch (trap_t &trap) {
    LOG(FATAL_S) << fmt::format("spike trapped with {}", trap.name());
  }
}

void VBridgeImpl::record_rf_accesses(const VrfWritePeek &rf_write) {
  int valid = rf_write.valid;
  int lane_idx = rf_write.lane_index;
  if (valid) {
    uint32_t vd = rf_write.request_vd;
    uint32_t offset = rf_write.request_offset;
    uint32_t mask = rf_write.request_mask;
    uint32_t data = rf_write.request_data;
    uint32_t idx = rf_write.request_instIndex;
    SpikeEvent *se_vrf_write = nullptr;
    for (auto se = to_rtl_queue.rbegin(); se != to_rtl_queue.rend(); se++) {
      if (se->issue_idx == idx) {
        se_vrf_write = &(*se);
      }
    }
    if (se_vrf_write == nullptr) {
      LOG(WARNING) << fmt::format("[{}] rtl detect vrf write which cannot find se, maybe from committed load insn (idx={})", get_t(), idx);
    } else if (!se_vrf_write->is_load) {
      add_rtl_write(se_vrf_write, lane_idx, vd, offset, mask, data, idx);
      LOG(INFO) << fmt::format("[{}] rtl detect vrf write (lane={}, vd={}, offset={}, mask={:04b}, data={:08X}, insn idx={})",
                               get_t(), lane_idx, vd, offset, mask, data, idx);
    }
  }  // end if(valid)
}

void VBridgeImpl::record_rf_queue_accesses(const VLsuWriteQueuePeek &lsu_queue) {
  bool valid = lsu_queue.write_valid;
  if (valid) {
    uint32_t vd = lsu_queue.request_data_vd;
    uint32_t offset = lsu_queue.request_data_offset;
    uint32_t mask = lsu_queue.request_data_mask;
    uint32_t data = lsu_queue.request_data_data;
    uint32_t idx = lsu_queue.request_data_instIndex;
    uint32_t targetLane = lsu_queue.request_targetLane;
    int lane_idx = __builtin_ctz(targetLane);
    SpikeEvent *se_vrf_write = nullptr;
    for (auto se = to_rtl_queue.rbegin(); se != to_rtl_queue.rend(); se++) {
      if (se->issue_idx == idx) {
        se_vrf_write = &(*se);
      }
    }
    LOG(INFO) << fmt::format("[{}] rtl detect vrf queue write (lane={}, vd={}, offset={}, mask={:04b}, data={:08X}, insn idx={})",
                             get_t(), lane_idx, vd, offset, mask, data, idx);
    CHECK_S(se_vrf_write != nullptr) << fmt::format("cannot find se with issue_idx {}", idx);
    add_rtl_write(se_vrf_write, lane_idx, vd, offset, mask, data, idx);
  }
}

void VBridgeImpl::add_rtl_write(SpikeEvent *se, uint32_t lane_idx, uint32_t vd, uint32_t offset, uint32_t mask, uint32_t data, uint32_t idx) {
  uint32_t record_idx_base = vd * consts::vlen_in_bytes + (lane_idx + consts::numLanes * offset) * 4;
  auto &all_writes = se->vrf_access_record.all_writes;

  for (int j = 0; j < 32 / 8; j++) {  // 32bit / 1byte
    if ((mask >> j) & 1) {
      uint8_t written_byte = (data >> (8 * j)) & 255;
      auto record_iter = all_writes.find(record_idx_base + j);

      if (record_iter != all_writes.end()) { // if find a spike write record
        auto &record = record_iter->second;
        CHECK_EQ_S((int) record.byte, (int) written_byte) << fmt::format(  // convert to int to avoid stupid printing
              ": [{}] {}th byte incorrect ({:02X} != {:02X}) for vrf write (lane={}, vd={}, offset={}, mask={:04b}) [vrf_idx={}]",
              get_t(), j, record.byte, written_byte, lane_idx, vd, offset, mask, record_idx_base + j);
        record.executed = true;

      } else if (uint8_t original_byte = vrf_shadow[record_idx_base + j]; original_byte != written_byte) {
//        CHECK_S(false) << fmt::format(": [{}] vrf writes {}th byte (lane={}, vd={}, offset={}, mask={:04b}, data={}, original_data={}), "
//                                      "but not recorded by spike ({}) [{}]",
//                                      get_t(), j, lane_idx, vd, offset, mask, written_byte,
//                                      original_byte, se->describe_insn(), record_idx_base + j);
        // TODO: check the case when the write not present in all_writes (require trace VRF data)
      } else {
        // no spike record and rtl written byte is identical as the byte before write, safe
      }

      vrf_shadow[record_idx_base + j] = written_byte;
    }  // end if mask
  }  // end for j
}

VBridgeImpl vbridge_impl_instance;
