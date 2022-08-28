#pragma once

#include <optional>

#include "Encoding.h"

namespace vmodel {

struct LaneState {
    // the first slot is of the highest priority, and only it can handle inter-lane transaction
    struct ExecSlot {
        Instr runningInstr;
        int groupIdx;
        int groupsNum;
        int vfuCycleRemaining;  // set to -1 if not waiting for vfu
        bool vs1Done:1, vs2Done:1, vfuDone:1, wbDone:1, allDone:1;
    } slots [params.num_slots];

    BusPacket posPacket;  // packet to lanes[i + 1]
    BusPacket negPacket;  // packet to lanes[i - 1]
};

struct CoreState {
    LaneState lanes[params.num_lanes];
    std::optional<Instr> scoreboard[params.num_slots];
};

bool Step(CoreState &core, const Instr &pendingInstr);

} // namespace vmodel
