#pragma once

#include <cstdint>
#include "VLane.h"

namespace vmodel {

struct LaneIn {
    bool ready;
    uint8_t index;
    uint8_t uop;
    uint8_t w;
    uint8_t n;
    uint8_t sew;
    uint8_t groupIndex;
    uint8_t sign;
    uint8_t maskOP2;
    uint8_t maskDestination;
    uint8_t rm;
    uint64_t src_0;
    uint64_t src_1;
    uint64_t src_2;

    void Feed(VLane &veriLane) const {
        veriLane.req_ready = this->ready;
        veriLane.req_bits_index = this->index;
        veriLane.req_bits_uop = this->uop;
        veriLane.req_bits_w = this->w;
        veriLane.req_bits_n = this->n;
        veriLane.req_bits_sew = this->sew;
        veriLane.req_bits_groupIndex = this->groupIndex;
        veriLane.req_bits_sign = this->sign;
        veriLane.req_bits_maskOP2 = this->maskOP2;
        veriLane.req_bits_maskDestination = this->maskDestination;
        veriLane.req_bits_rm = this->rm;
        veriLane.req_bits_src_0 = this->src_0;
        veriLane.req_bits_src_1 = this->src_1;
        veriLane.req_bits_src_2 = this->src_2;
    }
};

struct LaneOut {
    bool valid;
    uint64_t res;
    uint64_t carry;

    bool IsEqual(const VLane &veriLane) const {
        bool isEqual = true;
        if (veriLane.resp_valid != this->valid) isEqual = false;
        if (veriLane.resp_bits_carry != this->carry) isEqual = false;
        if (veriLane.resp_bits_res != this->res) isEqual = false;
        return isEqual;
    }
};

class LaneModel {
public:
    LaneOut Step(LaneIn in);
};

} // namespace vmodel
