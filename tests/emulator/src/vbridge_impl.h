#pragma once

#include <queue>
#include <optional>

#include "mmu.h"

#include "VV.h"
#include "verilated_fst_c.h"

#include "spike_event.h"
#include "simple_sim.h"
#include "rtl_event.h"
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
  explicit VBridgeImpl();

  ~VBridgeImpl();

  void setup(const std::string &bin, const std::string &wave, uint64_t reset_vector, uint64_t cycles);

  // todo remove this.
  void configure_simulator(int argc, char **argv);

  void run();

  uint8_t load(uint64_t address);

  uint64_t get_t();

private:
  VerilatedContext ctx;
  VV top;
  VerilatedFstC tfp;
  simple_sim sim;
  isa_parser_t isa;
  processor_t proc;
  uint64_t _cycles;
  std::map<reg_t, TLReqRecord> tl_banks[consts::numTL];

  /// to rtl stack
  /// in the spike thread, spike should detech if this queue is full, if not full, execute until a vector instruction,
  /// record the behavior of this instruction, and send to str_stack.
  /// in the RTL thread, the RTL driver will consume from this queue, drive signal based on the queue.
  /// size of this queue should be as big as enough to make rtl free to run, reducing the context switch overhead.
  std::list<SpikeEvent> to_rtl_queue;

  // in vrf_shadow we keep a duplicate of vrf in rtl, in order to detect unexpected vrf write
  std::unique_ptr<uint8_t[]> vrf_shadow;

  const size_t to_rtl_queue_size = 10;

  /// file path of executeable binary file, which will be executed.
  std::string bin;
  /// generated waveform path.
  std::string wave;
  /// reset vector of
  uint64_t reset_vector{};
  /// RTL timeout cycles
  /// note: this is not the real system cycles, scalar instructions is evaulated via spike, which is not recorded.
  uint64_t timeout{};

  // This flag will be set to true when we see a msimend write insn.
  bool is_spike_exited = false;

  inline void reset();

  insn_fetch_t fetch_proc_insn();

  std::optional<SpikeEvent> create_spike_event(insn_fetch_t fetch);

  void init_spike();
  void init_simulator();
  void terminate_simulator();

  std::optional<SpikeEvent> spike_step();

  void update_lsu_idx();
  void loop_until_se_queue_full();
  SpikeEvent *find_se_to_issue();

  // for load instructions, rtl would commit after vrf write requests enters the write queue (but not actually written),
  // hence for these instructions, we should record vrf write on queue, and ignore them on vrf.write
  void record_rf_queue_accesses();
  void record_rf_accesses();
  void add_rtl_write(SpikeEvent *se, uint32_t lane_idx, uint32_t vd, uint32_t offset, uint32_t mask, uint32_t data, uint32_t idx);
  void return_tl_response();
  void receive_tl_req();

  int get_mem_req_cycles() {
    return 1;
  };
};
