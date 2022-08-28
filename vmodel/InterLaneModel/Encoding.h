#pragma once

#include <cstddef>
#include <cstdint>

namespace vmodel {

constexpr struct {
    size_t num_lanes = 8;
    size_t num_vregs = 32;
    size_t num_ele = 32;
    size_t num_slots = 4;
    size_t num_banks = 4;

    size_t num_vrf_read_ports = 2;
    size_t num_vrf_write_ports = 1;

    size_t num_ele_per_lane = num_ele / num_lanes;  // 4
} params;

using Ele = uint32_t;

template <typename Val>
struct DecoupledIO {
    Val val;
    bool valid;
    bool ready;

    void Set(Val v) {
        val = v;
        valid = true;
    }
};

/// to indicate the VFU to use
enum class OpType {
    arithmetic,
    mul,
    div,
    popCount,
    ffo,
    getIndex,
    load,
    store,
};

/// a whole instruction
struct Instr {
    OpType opType;
    uint8_t vs1;
    uint8_t vs2;
    uint8_t vd;
    uint8_t eew;
    uint8_t vlen;
    uint8_t vmul;
};

/// the request/response sent on the bus
struct BusPacket {
    uint8_t targetVRF;
    uint8_t targetIdx;
    uint8_t eew;
    Ele value;
    bool isRequest;  // else is response
    bool valid;
};

}  // namespace vmodel
