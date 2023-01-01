#pragma once

#include <queue>
#include <optional>

#include <processor.h>
#include <mmu.h>

#ifdef COSIM_VERILATOR
#include <verilated_fst_c.h>
#endif

#include "simple_sim.h"
#include "vbridge_impl.h"
#include "encoding.h"
#include "vbridge_config.h"

class VBridgeImpl;

inline void clear_state(processor_t &proc) {
  auto *state = proc.get_state();
  state->log_reg_write.clear();
  state->log_mem_read.clear();
  state->log_mem_write.clear();
}

struct SpikeEvent {
  SpikeEvent(processor_t &proc, insn_fetch_t &fetch, VBridgeImpl *impl);

  [[nodiscard]] std::string describe_insn() const;

  void drive_rtl_req(const VInstrInterfacePoke &vinst) const;
  void drive_rtl_csr(const VCsrInterfacePoke &v_csr) const;

  void pre_log_arch_changes();
  void log_arch_changes();

  commit_log_mem_t mem_read_info;

  struct mem_log {
    uint64_t addr;
    uint64_t value;
    uint8_t size;
  };
  std::vector<mem_log> log_mem_queue;

  uint8_t lsu_idx = 255;
  uint8_t issue_idx = 255;
  processor_t &proc;
  VBridgeImpl *impl;
  std::string disasm;

  bool is_issued;

  bool is_load;
  bool is_store;
  bool is_widening;
  bool is_exit_insn;
  bool is_vfence_insn;

  uint64_t pc;
  uint32_t inst_bits;

  // scalar to vector interface(used for driver)
  uint32_t rs1_bits;
  uint32_t rs2_bits;
  uint32_t rd_idx;

  // vtype
  uint32_t vsew: 3;
  uint32_t vlmul: 3;
  bool vma: 1;
  bool vta: 1;
  uint32_t vxrm: 2;
  uint32_t vnf: 3;

  // other CSR
  bool vill: 1;
  bool vxsat: 1;

  /// range [XLEN-1:0].
  /// updated with vset{i}vl{i} and fault-only-first vector load instruction variants
  /// currently, we don't implement MMU, thus, no fault-only-first will be executed.
  uint32_t vl;
  uint16_t vstart;

  /// pipeline control signal with core
  bool _ignore_exception = false;  // TODO: give it correct value
  bool _store_buffer_clear = false;  // TODO: give it correct value

  struct vd_write_record_t {
    std::unique_ptr<uint8_t[]> vd_bytes;
  } vd_write_record;

  bool is_rd_written;
  uint32_t rd_bits;

  // returns {a, b} if the instruction may write vrf of index in range [a, a + b)
  [[nodiscard]] std::pair<uint32_t, uint32_t> get_vrf_write_range() const;

  struct {
    struct single_mem_write {
      uint32_t size_by_byte;
      reg_t val;
      bool executed = false; // set to true when rtl execute this mem access
    };
    struct single_mem_read {
      uint16_t size_by_byte;
      reg_t val;
      bool executed = false; // set to true when rtl execute this mem access
    };
    std::map<uint32_t, single_mem_write> all_writes;
    std::map<uint32_t, single_mem_read> all_reads;
  } mem_access_record;

  struct {
    struct single_vrf_write {
      uint8_t byte;
      bool executed = false; // set to true when rtl execute this mem access
    };
    // maps (vlen * bytes_per_vrf + byte_offset) to single_vrf_write
    std::map<uint32_t, single_vrf_write> all_writes;
  } vrf_access_record;

  void record_rd_write(const VRespInterface &v_resp);
  void check_is_ready_for_commit();
};
