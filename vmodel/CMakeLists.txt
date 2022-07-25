cmake_minimum_required(VERSION 3.20)
project(vmodel)
set(CMAKE_CXX_STANDARD 20)

include(FetchContent)
FetchContent_Declare(fmt GIT_REPOSITORY https://github.com/fmtlib/fmt GIT_TAG 8.0.1)
FetchContent_Declare(glog GIT_REPOSITORY https://github.com/google/glog GIT_TAG v0.5.0)
FetchContent_MakeAvailable(fmt glog)

find_package(verilator)
add_executable(Vlane src/LaneModel.cpp src/Main.cpp)
verilate(Vlane SOURCES ../builds/Lane.v)
target_link_libraries(Vlane fmt glog)
