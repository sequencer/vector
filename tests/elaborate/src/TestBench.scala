package tests.elaborate

import chisel3._
import chisel3.experimental.SerializableModuleGenerator
import v.{V, VParameter}

class TestBench extends RawModule {
  val clock = Wire(Clock())
  val reset = Wire(Bool())
  val generator = SerializableModuleGenerator(
    classOf[V],
    VParameter(
      xLen = 32,
      vLen = 1024,
      dataPathWidth = 32,
      laneNumer = 8,
      physicalAddressWidth = 32,
      chainingSize = 4,
      vrfWriteQueueSize = 4
    )
  )
  val dut = withClockAndReset(clock, reset) {
    Module(
      generator.module()
    )
  }
  val verificationModule = Module(new VerificationModule(dut))
  dut.request <> verificationModule.req
  dut.response <> verificationModule.resp
  dut.csrInterface <> verificationModule.csrInterface
  dut.storeBufferClear <> verificationModule.storeBufferClear
  dut.memoryPorts <> verificationModule.tlPort
  clock := verificationModule.clock
  reset := verificationModule.reset
}
