#pragma once

#include <queue>
#include <optional>
#include <mutex>
#include <thread>
#include <condition_variable>

#include <mmu.h>

#ifdef COSIM_VERILATOR
#include <verilated.h>
#include <verilated_fst_c.h>
#include <svdpi.h>
#endif

#include "spike_event.h"
#include "simple_sim.h"
#include "encoding.h"
#include "vbridge_config.h"

class SpikeEvent;

struct TLReqRecord {
  uint64_t data;
  uint32_t size_by_byte;
  uint16_t source;

  /// when opType set to nil, it means this record is already sent back
  enum class opType {
    Nil, Get, PutFullData
  } op;
  int remaining_cycles;

  TLReqRecord(uint64_t data, uint32_t size_by_byte, uint16_t source, opType op, int cycles) :
      data(data), size_by_byte(size_by_byte), source(source), op(op), remaining_cycles(cycles) {};
};

class VBridgeImpl {
public:
  VBridgeImpl();

  void dpiDumpWave();

  void dpiInitCosim();

  void timeoutCheck();

  uint8_t load(uint64_t address);

  uint64_t get_t();

  // Simulator Calls
#ifdef COSIM_VERILATOR
  uint64_t getCycle() {
    return ctx->time();
  }
  VerilatedCovContext* getCoverage() {
    return ctx->coveragep();
  }
#endif

  void dpiPokeInst(const VInstrInterfacePoke &v_instr, const VCsrInterfacePoke &v_csr, const VRespInterface &v_resp);
  void dpiPokeTL(const VTlInterfacePoke &v_tl_poke);
  void dpiPeekTL(const VTlInterface &v_tl);
  void dpiPeekWriteQueue(const VLsuWriteQueuePeek &v_enq);
  void dpiPeekIssue(svBit ready, svBitVecVal issueIdx);
  void dpiPeekLsuEnq(const VLsuReqEnqPeek &lsu_req_enq);
  void dpiPeekVrfWrite(const VrfWritePeek &v_enq);

private:
  simple_sim sim;
  isa_parser_t isa;
  processor_t proc;
  std::multimap<reg_t, TLReqRecord> tl_banks[consts::numTL];

  SpikeEvent *se_to_issue;

  /// to rtl stack
  /// in the spike thread, spike should detech if this queue is full, if not full, execute until a vector instruction,
  /// record the behavior of this instruction, and send to str_stack.
  /// in the RTL thread, the RTL driver will consume from this queue, drive signal based on the queue.
  /// size of this queue should be as big as enough to make rtl free to run, reducing the context switch overhead.
  std::list<SpikeEvent> to_rtl_queue;

  // in vrf_shadow we keep a duplicate of vrf in rtl, in order to detect unexpected vrf write
  std::unique_ptr<uint8_t[]> vrf_shadow;

  /// file path of executable binary file, which will be executed.
  const std::string bin = getenv("COSIM_bin");

  /// generated waveform path.
  const std::string wave = getenv("COSIM_wave");

  /// reset vector of
  const uint64_t reset_vector = std::stoul(getenv("COSIM_reset_vector"), nullptr, 16);

  /// RTL timeout cycles
  /// note: this is not the real system cycles, scalar instructions is evaulated via spike, which is not recorded.
  const uint64_t timeout = std::stoul(getenv("COSIM_timeout"));

  std::optional<SpikeEvent> create_spike_event(insn_fetch_t fetch);

  void init_spike();
  std::optional<SpikeEvent> spike_step();
  SpikeEvent *find_se_to_issue();

  // for load instructions, rtl would commit after vrf write requests enters the write queue (but not actually written),
  // hence for these instructions, we should record vrf write on queue, and ignore them on vrf.write
  void record_rf_queue_accesses(const VLsuWriteQueuePeek &lsu_queues);
  void record_rf_accesses(const VrfWritePeek &rf_writs);
  void return_tl_response(const VTlInterfacePoke &tl_poke);
  void receive_tl_req(const VTlInterface &tl);
  void update_lsu_idx(const VLsuReqEnqPeek &enq);

  void add_rtl_write(SpikeEvent *se, uint32_t lane_idx, uint32_t vd, uint32_t offset, uint32_t mask, uint32_t data, uint32_t idx);

  // simulator context
#ifdef COSIM_VERILATOR
  VerilatedContext *ctx;
  VerilatedFstC tfp;
#endif

  int get_mem_req_cycles() {
    return 1;
  };

  uint64_t last_commit_time = 0;
};

extern VBridgeImpl vbridge_impl_instance;
