#pragma once

#include "svdpi.h"

// Write this CSR to end simulation.
constexpr uint32_t CSR_MSIMEND = 0x7cc;

namespace TlOpcode {
constexpr int
    Get = 4,
    AccessAckData = 1,
    PutFullData = 0,
    PutPartialData = 1,
    AccessAck = 0;
}

struct VCsrInterfacePoke {
  svBitVecVal *vl;
  svBitVecVal *vStart;
  svBitVecVal *vlmul;
  svBitVecVal *vSew;
  svBitVecVal *vxrm;
  svBit *vta;
  svBit *vma;
  svBit *ignoreException;
};

struct VTlInterface {
  int channel_id;
  svBitVecVal a_bits_opcode;
  svBitVecVal a_bits_param;
  svBitVecVal a_bits_size;
  svBitVecVal a_bits_source;
  svBitVecVal a_bits_address;
  svBitVecVal a_bits_mask;
  svBitVecVal a_bits_data;
  svBit a_corrupt;
  svBit a_valid;
  svBit d_ready;
};

struct VTlInterfacePoke {
  int channel_id;
  svBitVecVal *d_bits_opcode;
  svBitVecVal *d_bits_param;
  svBitVecVal *d_bits_size;
  svBitVecVal *d_bits_source;
  svBitVecVal *d_bits_sink;
  svBitVecVal *d_bits_denied;
  svBitVecVal *d_bits_data;
  svBit *d_corrupt;
  svBit *d_valid;
  svBit *a_ready;

  svBit d_ready;
};

struct VInstrInterfacePoke {
  svBitVecVal *inst;
  svBitVecVal *src1Data;
  svBitVecVal *src2Data;
  svBit *valid;
};

struct VRespInterface {
  svBit valid;
  svBitVecVal bits;
};

struct VInstrFire {
  svBit ready;
  svBitVecVal index;
};

struct VLsuWriteQueuePeek {
  int mshr_index;
  svBit write_valid;
  svBitVecVal request_data_vd;
  svBitVecVal request_data_offset;
  svBitVecVal request_data_mask;
  svBitVecVal request_data_data;
  svBitVecVal request_data_instIndex;
  svBitVecVal request_targetLane;
};

struct VrfWritePeek {
  int lane_index;
  svBit valid;
  svBitVecVal request_vd;
  svBitVecVal request_offset;
  svBitVecVal request_mask;
  svBitVecVal request_data;
  svBitVecVal request_instIndex;
};

struct VLsuReqEnqPeek {
  svBitVecVal enq;
};
