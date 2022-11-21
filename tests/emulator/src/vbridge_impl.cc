#include <fmt/core.h>
#include <glog/logging.h>

#include "disasm.h"

#include "verilated.h"

#include "exceptions.h"
#include "vbridge_impl.h"
#include "util.h"
#include "rtl_event.h"
#include "vpi.h"
#include "tl_interface.h"
#include "glog_exception_safe.h"
#include "encoding.h"

/// convert TL style size to size_by_bytes
inline uint32_t decode_size(uint32_t encoded_size) {
  return 1 << encoded_size;
}

void VBridgeImpl::reset() {
  top.clock = 0;
  top.reset = 1;
  top.eval();
  tfp.dump(0);

  // posedge
  top.clock = 1;
  top.eval();
  tfp.dump(1);

  // negedge
  top.reset = 0;
  top.clock = 0;
  top.eval();
  tfp.dump(2);
  // posedge
  top.reset = 0;
  top.clock = 1;
  top.eval();
  tfp.dump(3);
  ctx.time(2);
}

void VBridgeImpl::setup(const std::string &_bin, const std::string &_wave, uint64_t _reset_vector, uint64_t cycles) {
  this->bin = _bin;
  this->wave = _wave;
  this->reset_vector = _reset_vector;
  this->timeout = cycles;
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

VBridgeImpl::~VBridgeImpl() {
  terminate_simulator();
}

void VBridgeImpl::configure_simulator(int argc, char **argv) {
  ctx.commandArgs(argc, argv);
}

void VBridgeImpl::init_spike() {
  // reset spike CPU
  proc.reset();
  // load binary to reset_vector
  sim.load(bin, reset_vector);
}

void VBridgeImpl::init_simulator() {
  Verilated::traceEverOn(true);
  top.trace(&tfp, 99);
  tfp.open(wave.c_str());
  _cycles = timeout;
}

void VBridgeImpl::terminate_simulator() {
  if (tfp.isOpen()) {
    tfp.close();
  }
  top.final();
}

uint64_t VBridgeImpl::get_t() {
  return ctx.time();
}

std::optional<SpikeEvent> VBridgeImpl::spike_step() {
  auto state = proc.get_state();
  auto fetch = proc.get_mmu()->load_insn(state->pc);
  auto event = create_spike_event(fetch);  // event not empty iff fetch is v inst
  auto &xr = proc.get_state()->XPR;

  reg_t pc;
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
  bool v_type = opcode == 0b1010111 && width != 0b111;

  bool is_csr_type = opcode == 0b1110011 && (width & 0b011);
  bool is_csr_write = is_csr_type && ((width & 0b100) | rs1);

  if (is_load_type || is_store_type || v_type || (
      is_csr_write && csr == CSR_MSIMEND)) {
    return SpikeEvent{proc, fetch, this};
  } else {
    return {};
  }
}

uint8_t VBridgeImpl::load(uint64_t address){
  return *sim.addr_to_mem(address);
}

void VBridgeImpl::run() {

  init_spike();
  init_simulator();
  reset();

  // start loop
  while (true) {
    // spike =======> to_rtl_queue =======> rtl
    loop_until_se_queue_full();

    // loop while there exists unissued insn in queue
    while (!to_rtl_queue.front().is_issued) {
      // in the RTL thread, for each RTL cycle, valid signals should be checked, generate events, let testbench be able
      // to check the correctness of RTL behavior, benchmark performance signals.
      SpikeEvent *se_to_issue = find_se_to_issue();

      se_to_issue->drive_rtl_req(top);
      se_to_issue->drive_rtl_csr(top);

      return_tl_response();

      // Make sure any combinatorial logic depending upon inputs that may have changed before we called tick() has settled before the rising edge of the clock.
      top.clock = 1;
      top.eval();

      // Instruction is_issued, top.req_ready deps on top.req_bits_inst
      if (top.req_ready) {
        se_to_issue->is_issued = true;
        se_to_issue->issue_idx = vpi_get_integer("TOP.V.instCount");
        LOG(INFO) << fmt::format("[{}] issue to rtl ({}), issue index={}", get_t(), se_to_issue->describe_insn(), se_to_issue->issue_idx);
      }

      receive_tl_req();

      // negedge
      top.clock = 0;
      top.eval();
      tfp.dump(2 * ctx.time());
      ctx.timeInc(1);

      record_rf_queue_accesses();

      // posedge, update registers
      top.clock = 1;
      top.eval();
      tfp.dump(2 * ctx.time() - 1);

      update_lsu_idx();
      record_rf_accesses();

      if (top.resp_valid) {
        SpikeEvent &se = to_rtl_queue.back();
        se.record_rd_write(top);
        se.check_is_ready_for_commit();
        LOG(INFO) << fmt::format("[{}] rtl commit insn ({})", get_t(), to_rtl_queue.back().describe_insn());
        to_rtl_queue.pop_back();
      }

      if (get_t() >= timeout) {
        throw TimeoutException();
      }
    }

    LOG(INFO) << fmt::format("[{}] all insn in to_rtl_queue are issued", get_t());
    if (is_spike_exited) {
      // TODO: loop while there exists (uncommitted) insn in queue.
      LOG(INFO) << fmt::format("exiting spike");
      return;
    } else {
      LOG(INFO) << fmt::format("restarting spike");
    }
  }
}

void VBridgeImpl::receive_tl_req() {
#define TL(i, name) (get_tl_##name(top, (i)))
  for (int tlIdx = 0; tlIdx < 2; tlIdx++) {
    if (!TL(tlIdx, a_valid)) continue;

    uint8_t opcode = TL(tlIdx, a_bits_opcode);
    uint32_t addr = TL(tlIdx, a_bits_address);
    uint8_t size = TL(tlIdx, a_bits_size);
    uint16_t src = TL(tlIdx, a_bits_source);   // MSHR id, TODO: be returned in D channel
    uint32_t lsu_index = TL(tlIdx, a_bits_source) & 3;
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
      LOG(INFO) << fmt::format("[{}] receive rtl mem get req (addr={}, size={}byte), should return data {}",
                               get_t(), addr, decode_size(size), data);
      tl_banks[tlIdx].emplace(std::make_pair(addr, TLReqRecord{
          data, 1u << size, src, TLReqRecord::opType::Get, get_mem_req_cycles()
      }));
      mem_read->second.executed = true;
      break;
    }

    case TlOpcode::PutFullData: {
      uint32_t data = TL(tlIdx, a_bits_data);
      LOG(INFO) << fmt::format("[{}] receive rtl mem put req (addr={:08X}, size={}byte, data={})",
                               addr, decode_size(size), data);
      auto mem_write = se->mem_access_record.all_writes.find(addr);

      CHECK_S(mem_write != se->mem_access_record.all_writes.end())
              << fmt::format(": [{}] cannot find mem write of addr={:08X}", get_t(), addr);
      CHECK_EQ_S(mem_write->second.size_by_byte, decode_size(size)) << fmt::format(
          ": [{}] expect mem write of size {}, actual size {} (addr={:08X}, insn='{}')",
          get_t(), mem_write->second.size_by_byte, 1 << decode_size(size), addr, se->describe_insn());
      CHECK_EQ_S(mem_write->second.val, data) << fmt::format(
          ": [{}] expect mem write of data {}, actual data {} (addr={:08X}, insn='{}')",
          get_t(), mem_write->second.size_by_byte, 1 << decode_size(size), addr, se->describe_insn());

      tl_banks[tlIdx].emplace(std::make_pair(addr, TLReqRecord{
          data, 1u << size, src, TLReqRecord::opType::PutFullData, get_mem_req_cycles()
      }));
      mem_write->second.executed = true;
      break;
    }
    default: {
      LOG(FATAL) << fmt::format("unknown tl opcode {}", opcode);
    }
    }
  }
#undef TL
}

void VBridgeImpl::return_tl_response() {
#define TL(i, name) (get_tl_##name(top, (i)))
  for (int i = 0; i < consts::numTL; i++) {
    // update remaining_cycles
    for (auto &[addr, record]: tl_banks[i]) {
      if (record.remaining_cycles > 0) record.remaining_cycles--;
    }

    // find a finished request and return
    bool d_valid = false;
    for (auto &[addr, record]: tl_banks[i]) {
      if (record.remaining_cycles == 0) {
        TL(i, d_bits_opcode) = record.op == TLReqRecord::opType::Get ? TlOpcode::AccessAckData : TlOpcode::AccessAck;
        TL(i, d_bits_data) = record.data;
        TL(i, d_bits_sink) = record.source;
        d_valid = true;
        if (TL(i, d_ready)) {
          record.op = TLReqRecord::opType::Nil;
        }
        break;
      }
    }
    TL(i, d_valid) = d_valid;

    // collect garbage
    erase_if(tl_banks[i], [](const auto &record) {
      return record.second.op == TLReqRecord::opType::Nil;
    });

    // welcome new requests all the time
    TL(i, a_ready) = true;
  }
#undef TL
}

void VBridgeImpl::update_lsu_idx() {
  uint32_t lsuReqs[consts::numMSHR];
  for (int i = 0; i < consts::numMSHR; i++) {
    lsuReqs[i] = vpi_get_integer(fmt::format("TOP.V.lsu.reqEnq_debug_{}", i).c_str());
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

void VBridgeImpl::loop_until_se_queue_full() {
  while (to_rtl_queue.size() < to_rtl_queue_size) {
    try {
      if (auto spike_event = spike_step()) {
        SpikeEvent &se = spike_event.value();

        if (se.is_exit_insn) {
          is_spike_exited = true;
          LOG(INFO) << fmt::format("spike encounter exit insn, stop enqueuing.");
          return;
        }

        to_rtl_queue.push_front(std::move(se));
      }
    } catch (trap_t &trap) {
      LOG(FATAL) << fmt::format("spike trapped with {}", trap.name());
    }
  }
  LOG(INFO) << fmt::format("to_rtl_queue is full now, start to simulate.");
}

SpikeEvent *VBridgeImpl::find_se_to_issue() {
  SpikeEvent *se_to_issue = nullptr;
  for (auto iter = to_rtl_queue.rbegin(); iter != to_rtl_queue.rend(); iter++) {
    if (!iter->is_issued) {
      se_to_issue = &(*iter);
      break;
    }
  }
  CHECK(se_to_issue) << fmt::format("[{}] all events in to_rtl_queue are is_issued", get_t());  // TODO: handle this
  return se_to_issue;
}

void VBridgeImpl::record_rf_accesses() {
  for (int lane_idx = 0; lane_idx < consts::numLanes; lane_idx++) {
    int valid = vpi_get_integer(fmt::format("TOP.V.laneVec_{}.vrf.write_valid", lane_idx).c_str());
    if (valid) {
      uint32_t vd = vpi_get_integer(fmt::format("TOP.V.laneVec_{}.vrf.write_bits_vd", lane_idx).c_str());
      uint32_t offset = vpi_get_integer(fmt::format("TOP.V.laneVec_{}.vrf.write_bits_offset", lane_idx).c_str());
      uint32_t mask = vpi_get_integer(fmt::format("TOP.V.laneVec_{}.vrf.write_bits_mask", lane_idx).c_str());
      uint32_t data = vpi_get_integer(fmt::format("TOP.V.laneVec_{}.vrf.write_bits_data", lane_idx).c_str());
      uint32_t idx = vpi_get_integer(fmt::format("TOP.V.laneVec_{}.vrf.write_bits_instIndex", lane_idx).c_str());
      SpikeEvent *se_vrf_write = nullptr;
      for (auto se = to_rtl_queue.rbegin(); se != to_rtl_queue.rend(); se++) {
        if (se->issue_idx == idx) {
          se_vrf_write = &(*se);
        }
      }
      if (se_vrf_write == nullptr) {
        LOG(INFO) << fmt::format("[{}] rtl detect vrf write which cannot find se, maybe from committed load insn", get_t());
      } else if (!se_vrf_write->is_load) {
        add_rtl_write(se_vrf_write, lane_idx, vd, offset, mask, data, idx);
        LOG(INFO) << fmt::format("[{}] rtl detect vrf write (lane={}, vd={}, offset={}, mask={:04b}, data={:08X}, insn idx={})",
                                 get_t(), lane_idx, vd, offset, mask, data, idx);
      }
    }  // end if(valid)
  }  // end for lane_idx
}

void VBridgeImpl::record_rf_queue_accesses() {
  for (int queueIdx = 0; queueIdx < consts::numMSHR; queueIdx++) {
    bool valid = vpi_get_integer(fmt::format("TOP.V.lsu.writeQueueVec_{}.io_enq_valid", queueIdx).c_str());
    if (valid) {
      uint32_t vd = vpi_get_integer(fmt::format("TOP.V.lsu.writeQueueVec_{}.io_enq_bits_data_vd", queueIdx).c_str());
      uint32_t offset = vpi_get_integer(fmt::format("TOP.V.lsu.writeQueueVec_{}.io_enq_bits_data_offset", queueIdx).c_str());
      uint32_t mask = vpi_get_integer(fmt::format("TOP.V.lsu.writeQueueVec_{}.io_enq_bits_data_mask", queueIdx).c_str());
      uint32_t data = vpi_get_integer(fmt::format("TOP.V.lsu.writeQueueVec_{}.io_enq_bits_data_data", queueIdx).c_str());
      uint32_t idx = vpi_get_integer(fmt::format("TOP.V.lsu.writeQueueVec_{}.io_enq_bits_data_instIndex", queueIdx).c_str());
      uint32_t targetLane = vpi_get_integer(fmt::format("TOP.V.lsu.writeQueueVec_{}.io_enq_bits_targetLane", queueIdx).c_str());
      int lane_idx = __builtin_ctz(targetLane);
      SpikeEvent *se_vrf_write = nullptr;
      for (auto se = to_rtl_queue.rbegin(); se != to_rtl_queue.rend(); se++) {
        if (se->issue_idx == idx) {
          se_vrf_write = &(*se);
        }
      }
      CHECK_S(se_vrf_write != nullptr);
      LOG(INFO) << fmt::format("[{}] rtl detect vrf queue write (lane={}, vd={}, offset={}, mask={:04b}, data={:08X}, insn idx={})",
                               get_t(), lane_idx, vd, offset, mask, data, idx);
      add_rtl_write(se_vrf_write, lane_idx, vd, offset, mask, data, idx);
    }
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
              ": [{}] byte {} incorrect for vrf write (lane={}, vd={}, offset={}, mask={:04b}) [{}]",
              get_t(), j, lane_idx, vd, offset, mask, record_idx_base + j);
        record.executed = true;

      } else if (uint8_t original_byte = vrf_shadow[record_idx_base + j]; original_byte != written_byte) {
        CHECK_S(false) << fmt::format(": [{}] vrf writes byte {} (lane={}, vd={}, offset={}, mask={:04b}, data={}, original_data={}), "
                                      "but not recorded by spike [{}]",
                                      get_t(), j, lane_idx, vd, offset, mask, written_byte,
                                      original_byte, record_idx_base + j);
        // TODO: check the case when the write not present in all_writes (require trace VRF data)
      } else {
        // no spike record and rtl written byte is identical as the byte before write, safe
      }

      vrf_shadow[record_idx_base + j] = written_byte;
    }  // end if mask
  }  // end for j
}
