#ifdef COSIM_VERILATOR
#include <VTestBench__Dpi.h>
#elif COSIM_VCS
#include "vc_hdrs.h"
#endif

#include <csignal>

#include <glog/logging.h>
#include <fmt/core.h>

#include "encoding.h"
#include "svdpi.h"
#include "vbridge_impl.h"
#include "exceptions.h"

static bool terminated = false;

static void setScope() {
#ifdef COSIM_VERILATOR
  svSetScope(svGetScopeFromName("TOP.TestBench.verificationModule.verbatim"));
#elif COSIM_VCS
  svSetScope(svGetScopeFromName("TestBench.verificationModule.verbatim"));
#endif
};

static void sigint_handler(int s) {
  terminated = true;
  setScope();
  dpiFinish();
}

#define TRY(action) \
  try {             \
    if (!terminated) {action}          \
  } catch (ReturnException &e) { \
    terminated = true;                \
    LOG(INFO) << fmt::format("detect returning instruction, gracefully quit simulation");                  \
    setScope(); \
    dpiFinish();                  \
  } catch (std::runtime_error &e) { \
    terminated = true;                \
    LOG(ERROR) << fmt::format("detect exception ({}), gracefully abort simulation", e.what());                 \
    setScope(); \
    dpiError(e.what());  \
  }

void VBridgeImpl::dpiDumpWave() {
  TRY({
    setScope();
    ::dpiDumpWave((wave + ".fst").c_str());
  })
}

[[maybe_unused]] void dpiInitCosim() {
  std::signal(SIGINT, sigint_handler);
  TRY({
    vbridge_impl_instance.dpiInitCosim();
  })
}

[[maybe_unused]] void dpiPeekIssue(svBit ready, const svBitVecVal *issueIdx) {
  TRY({
    vbridge_impl_instance.dpiPeekIssue(ready, *issueIdx);
  })
}

[[maybe_unused]] void dpiPokeInst(
    svBitVecVal *request_inst,
    svBitVecVal *request_src1Data,
    svBitVecVal *request_src2Data,
    svBit *instValid,

    svBitVecVal *vl,
    svBitVecVal *vStart,
    svBitVecVal *vlmul,
    svBitVecVal *vSew,
    svBitVecVal *vxrm,
    svBit *vta,
    svBit *vma,
    svBit *ignoreException,

    svBit respValid,
    const svBitVecVal *response_data
) {
  TRY({
    vbridge_impl_instance.dpiPokeInst(
        VInstrInterfacePoke{request_inst, request_src1Data, request_src2Data, instValid},
        VCsrInterfacePoke{vl, vStart, vlmul, vSew, vxrm, vta, vma, ignoreException},
        VRespInterface{respValid, *response_data}
    );
  })
}

[[maybe_unused]] void dpiPeekTL(
    int channel_id,
    const svBitVecVal *a_opcode,
    const svBitVecVal *a_param,
    const svBitVecVal *a_size,
    const svBitVecVal *a_source,
    const svBitVecVal *a_address,
    const svBitVecVal *a_mask,
    const svBitVecVal *a_data,
    svBit a_corrupt,
    svBit a_valid,
    svBit d_ready
) {
  TRY({
    vbridge_impl_instance.dpiPeekTL(
        VTlInterface{channel_id, *a_opcode, *a_param, *a_size, *a_source, *a_address, *a_mask, *a_data,
                     a_corrupt, a_valid, d_ready});
  })
}

[[maybe_unused]] void dpiPokeTL(
    int channel_id,
    svBitVecVal *d_opcode,
    svBitVecVal *d_param,
    svBitVecVal *d_size,
    svBitVecVal *d_source,
    svBitVecVal *d_sink,
    svBitVecVal *d_denied,
    svBitVecVal *d_data,
    svBit *d_corrupt,
    svBit *d_valid,
    svBit *a_ready,
    svBit d_ready
) {
  TRY({
    vbridge_impl_instance.dpiPokeTL(
        VTlInterfacePoke{channel_id, d_opcode, d_param, d_size, d_source, d_sink, d_denied, d_data,
                         d_corrupt, d_valid, a_ready, d_ready});
  })
}

[[maybe_unused]] void dpiPeekLsuEnq(const svBitVecVal *enq) {
  TRY({
    vbridge_impl_instance.dpiPeekLsuEnq(VLsuReqEnqPeek{*enq});
  })
}

[[maybe_unused]] void dpiPeekWriteQueue(
    int mshr_index,
    svBit write_valid,
    const svBitVecVal *request_data_vd,
    const svBitVecVal *request_data_offset,
    const svBitVecVal *request_data_mask,
    const svBitVecVal *request_data_data,
    const svBitVecVal *request_data_instIndex,
    const svBitVecVal *request_targetLane
) {
  TRY({
    vbridge_impl_instance.dpiPeekWriteQueue(
        VLsuWriteQueuePeek{mshr_index, write_valid, *request_data_vd, *request_data_offset,
                           *request_data_mask, *request_data_data, *request_data_instIndex,
                           *request_targetLane});
  })
}

[[maybe_unused]] void dpiPeekVrfWrite(
    int lane_idx,
    svBit valid,
    const svBitVecVal *request_vd,
    const svBitVecVal *request_offset,
    const svBitVecVal *request_mask,
    const svBitVecVal *request_data,
    const svBitVecVal *request_instIndex
) {
  TRY({
    vbridge_impl_instance.dpiPeekVrfWrite(VrfWritePeek{lane_idx, valid, *request_vd, *request_offset,
                                                       *request_mask, *request_data, *request_instIndex});
  })
}

[[maybe_unused]] void dpiTimeoutCheck() {
  TRY({
    vbridge_impl_instance.timeoutCheck();
  })
}
