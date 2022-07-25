#include <fmt/core.h>
#include <glog/logging.h>
#include <verilated.h>

// verilator
#include "VLane.h"

#include "LaneModel.h"

int main(int argc, char** argv) {
    google::InitGoogleLogging(argv[0]);

    VerilatedContext context;
    context.commandArgs(argc, argv);
    VLane lane{&context};
    vmodel::LaneModel laneModel;

    int64_t t = 0;
    while (!context.gotFinish()) {
        t++;

        vmodel::LaneIn input{};
        input.Feed(lane);
        lane.eval();

        auto output = laneModel.Step(input);
        if (!output.IsEqual(lane)) {
            LOG(ERROR) << fmt::format("unexpected output in time {}", t);
        }
    }
    return 0;
}