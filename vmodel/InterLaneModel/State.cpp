#include <utility>

#include "State.h"

namespace vmodel {

/// return: regLineIdx, bankIdx
std::pair<int, int> calcRegIdx(int vs, int groupIdx, int eew) {
    constexpr size_t linesPerReg = params.num_ele_per_lane / params.num_banks * 4;  // 4 (const 4 = 32 / 8)
    return {vs * linesPerReg + (groupIdx >> eew), groupIdx & ((1 << eew) - 1)};
};

constexpr int regLinesPerReg = params.num_ele_per_lane * params.num_banks / 4;  // 4 (4 = 32 / 8)

bool Step(CoreState &core, const Instr &pendingInstr) {
    bool canIssueInstr = true;  // if all slot is available, no hazard, scoreboard not full
    for (auto &lane : core.lanes) {
        // TODO: determine hazard
        bool laneIsAvailable = false;  // if any slot is available
        for (auto &slot : lane.slots) {
            if (slot.allDone) {
                laneIsAvailable = true;
                break;
            }
        }
        if (!laneIsAvailable) {
            canIssueInstr = false;
            break;
        }
    }

    bool noRemainingScoreboard = true;
    for (auto &instr : core.scoreboard) {
        if (instr.has_value()) {
            noRemainingScoreboard = false;
            instr = pendingInstr;
            break;
        }
    }
    if (noRemainingScoreboard) canIssueInstr = false;

    if (canIssueInstr) {
        for (auto &lane : core.lanes) {
            for (auto &slot: lane.slots) {
                if (slot.allDone) {
                    // issue instr to the slot
                    slot.runningInstr = pendingInstr;
                    slot.groupIdx = 0;
                    slot.groupsNum = params.num_ele / params.num_lanes;   // TODO: consider vl
                    slot.vs1Done = slot.vs2Done = slot.vfuDone = slot.allDone = false;
                }
            }
        }
    }

    BusPacket busPacketsForLane[params.num_lanes][2];
    for (int i = 0; i < params.num_lanes; i++) {
        busPacketsForLane[i][0] = core.lanes[(i + params.num_lanes - 1) % params.num_lanes].posPacket;
        busPacketsForLane[i][1] = core.lanes[(i + 1) % params.num_lanes].posPacket;
    }

    for (auto &lane : core.lanes) {
        /// lifecycle of a uOp
        /// - READ: !(vs1Done && vs2Done)
        /// - EXE: !vfuDone
        /// - WB: !wbDone
        for (int i = 0; i < params.num_slots; i++) {
            auto &slot = lane.slots[i];
            auto &instr = slot.runningInstr;
            // TODO: handle inter-lane

            if (!(slot.vs1Done && slot.vs2Done)) {  // READ
                // arbitrate read slot
                if (i == 0) {
                    slot.vs1Done = slot.vs2Done = true;
                } else {
                    if (!slot.vs1Done) {
                        slot.vs1Done = true;
                    } else {
                        slot.vs2Done = true;
                    }
                }
                if (slot.vs1Done && slot.vs2Done) {  // ready to EXE
                    slot.vfuCycleRemaining = 3;  // TODO: adjust cycles
                }

            } else if (!slot.vfuDone) {   // EXE
                slot.vfuCycleRemaining--;
                if (slot.vfuCycleRemaining == 0) slot.vfuDone = true;  // ready to WB

            } else if (!slot.wbDone) {   // WB
                slot.wbDone = true;
                if (slot.wbDone) {
                    slot.groupIdx++;
                    if (slot.groupIdx >= slot.groupsNum) {
                        slot.allDone = true;
                    }
                }
            }
        }  // end for slot in slots

        // squeeze slot frontward
        for (int i = 0, j = 0; i < params.num_slots; i++) {
            if (!lane.slots[i].allDone) {
                if (i != j) {
                    lane.slots[j] = lane.slots[i];
                }
                j++;
            }
        }
    } // end for lane in lanes

    return canIssueInstr;
}

} // namespace vmodel