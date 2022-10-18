#ifndef V_RTLEVENT_H
#define V_RTLEVENT_H

#include <queue>
#include <optional>

#include "VV.h"
#include "verilated_fst_c.h"
#include "mmu.h"
#include "simple_sim.h"

class RTLEvent {
public:
  void request_ready(bool signal);
  void commit_ready(bool signal);
  bool request();
  bool commit();
private:
  // ready to accept new instructions
  bool _req_ready;

  // Vector to Scalar register write back
  bool _resp_valid;
  uint32_t _resp_data;

  // Memroy load request
  bool _load_valid;
  uint64_t _load_base_address;

  // Memory store request(used for difftest)
  bool _store_valid;
  uint64_t _store_base_address;
  std::vector<uint32_t> _store_data;

  // VRF store event(used for difftest, TODO finish them)
  bool _vrf_write_valid;

};


#endif //V_RTLEVENT_H