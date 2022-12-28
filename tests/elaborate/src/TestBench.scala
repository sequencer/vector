package tests.elaborate

import chisel3._
import v.{V, VParam}

class TestBench extends RawModule {
  val clock = Wire(Clock())
  val reset = Wire(Bool())
  val dut = withClockAndReset(clock, reset) {
    Module(new V(VParam()))
  }
  withClockAndReset(clock, reset) {
    val coverModule = Module(new CoverModule(dut))
  }
  val monitor = Module(new Monitor(dut))

  val verificationModule = Module(new VerificationModule(dut))
  dut.req <> verificationModule.req
  dut.resp <> verificationModule.resp
  dut.csrInterface <> verificationModule.csrInterface
  dut.storeBufferClear <> verificationModule.storeBufferClear
  dut.tlPort <> verificationModule.tlPort
  clock := verificationModule.clock
  reset := verificationModule.reset
}
