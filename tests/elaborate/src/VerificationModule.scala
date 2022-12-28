package tests.elaborate

import chisel3._
import chisel3.experimental.ExtModule
import chisel3.experimental.hierarchy._
import chisel3.util.{log2Ceil, Decoupled, DecoupledIO, HasExtModuleInline, Valid, ValidIO}
import tilelink.{TLBundle, TLChannelA}
import v.{LSUWriteQueueBundle, LaneCsrInterface, V, VRFWriteRequest, VRequest, VResponse}

class VerificationModule(dut: V) extends TapModule {
  override val desiredName = "VerificationModule"
  val clock = IO(Output(Clock()))
  val reset = IO(Output(Bool()))

  val clockRate = 5

  val latPeekLsuEnq = 1
  val latPeekVrfWrite = 1
  val latPokeInst = 1
  val latPokeTL = 1

  val latPeekIssue = 2 // get se_to_issue here
  val latPeekTL = 2

  val negLatPeekWriteQueue = 1

  val verbatim = Module(new ExtModule with HasExtModuleInline {
    override val desiredName = "Verbatim"
    val clock = IO(Output(Clock()))
    val reset = IO(Output(Bool()))
    setInline(
      "verbatim.sv",
      s"""module Verbatim(
         |  output clock,
         |  output reset
         |);
         |  reg _clock = 1'b0;
         |  always #($clockRate) _clock = ~_clock;
         |  reg _reset = 1'b1;
         |  initial #(${2 * clockRate + 1}) _reset = 0;
         |
         |  assign clock = _clock;
         |  assign reset = _reset;
         |
         |  import "DPI-C" function void dpiInitCosim();
         |  initial dpiInitCosim();
         |
         |  import "DPI-C" function void dpiTimeoutCheck();
         |  always #(${2 * clockRate + 1}) dpiTimeoutCheck();
         |
         |  export "DPI-C" function dpiDumpWave;
         |  function dpiDumpWave(input string file);
         |   $$dumpfile(file);
         |   $$dumpvars(2, $$root.dut);
         |  endfunction;
         |
         |  export "DPI-C" function dpiFinish;
         |  function dpiFinish();
         |   $$finish;
         |  endfunction;
         |
         |  export "DPI-C" function dpiError;
         |  function dpiError(input string what);
         |   $$error(what);
         |  endfunction;
         |
         |endmodule
         |""".stripMargin
    )
  })
  clock := verbatim.clock
  reset := verbatim.reset

  // clone IO from V(I need types)
  val req:              DecoupledIO[VRequest] = IO(Decoupled(new VRequest(dut.parameter.xLen)))
  val resp:             ValidIO[VResponse] = IO(Flipped(Valid(new VResponse(dut.parameter.xLen))))
  val csrInterface:     LaneCsrInterface = IO(Output(new LaneCsrInterface(dut.parameter.laneParam.vlWidth)))
  val storeBufferClear: Bool = IO(Output(Bool()))
  val tlPort:           Vec[TLBundle] = IO(Vec(dut.parameter.memoryBankSize, Flipped(dut.parameter.tlParam.bundle())))
  storeBufferClear := true.B

  // XMR
  val laneWriteReadySeq = dut.laneVec.map(_.vrf.write.ready).map(tap)
  val laneWriteValidSeq = dut.laneVec.map(_.vrf.write.valid).map(tap)
  val laneWriteBitsSeq = dut.laneVec.map(_.vrf.write.bits).map(tap)

  val lsuReqEnqDbg = withClockAndReset(clock, reset)(RegNext(tap(dut.lsu.reqEnq)))
  val lsuReqEnqPeek = Module(new ExtModule with HasExtModuleInline {
    override val desiredName = "dpiPeekLsuEnq"
    val clock = IO(Input(Clock()))
    val numMshr = dut.parameter.lsuParam.mshrSize
    val enq = IO(Input(UInt(numMshr.W)))
    setInline(
      s"$desiredName.sv",
      s"""module $desiredName(
         |  input clock,
         |  input bit[${enq.getWidth - 1}:0] enq
         |);
         |import "DPI-C" function void $desiredName(
         |  input bit[${enq.getWidth - 1}:0] enq
         |);
         |always @ (posedge clock) #($latPeekLsuEnq) $desiredName(enq);
         |endmodule
         |""".stripMargin
    )
  })
  lsuReqEnqPeek.clock := clock
  lsuReqEnqPeek.enq := lsuReqEnqDbg.asUInt

  dut.lsu.writeQueueVec.zipWithIndex.foreach {
    case (mshr, i) =>
      val enq_data = tap(mshr.io.enq.bits.data)
      val targetLane = tap(mshr.io.enq.bits.targetLane)
      val peekWriteQueue = Module(new ExtModule with HasExtModuleInline {
        override val desiredName = "dpiPeekWriteQueue"
        val clock = IO(Input(Clock()))
        val data = IO(Input(new VRFWriteRequest(dut.parameter.vrfParam)))
        val writeValid = IO(Input(Bool()))
        val targetLane = IO(Input(UInt(dut.parameter.laneNumer.W)))
        val mshrIdx = IO(Input(UInt(32.W)))
        setInline(
          s"$desiredName.sv",
          s"""module $desiredName(
             |  input clock,
             |  input bit writeValid,
             |  input int mshrIdx,
             |  input bit[${enq_data.vd.getWidth - 1}:0] data_vd,
             |  input bit[${enq_data.offset.getWidth - 1}:0] data_offset,
             |  input bit[${enq_data.mask.getWidth - 1}:0] data_mask,
             |  input bit[${enq_data.data.getWidth - 1}:0] data_data,
             |  input bit[${enq_data.instructionIndex.getWidth - 1}:0] data_instructionIndex,
             |  input bit[${enq_data.last.getWidth - 1}:0] data_last,
             |  input bit[${targetLane.getWidth - 1}:0] targetLane
             |);
             |import "DPI-C" function void $desiredName(
             |  input int mshr_index,
             |  input bit write_valid,
             |  input bit[${enq_data.vd.getWidth - 1}:0] data_vd,
             |  input bit[${enq_data.offset.getWidth - 1}:0] data_offset,
             |  input bit[${enq_data.mask.getWidth - 1}:0] data_mask,
             |  input bit[${enq_data.data.getWidth - 1}:0] data_data,
             |  input bit[${enq_data.instructionIndex.getWidth - 1}:0] data_instructionIndex,
             |  input bit[${targetLane.getWidth - 1}:0] targetLane
             |);
             |always @ (negedge clock) #($negLatPeekWriteQueue) $desiredName(
             |  mshrIdx,
             |  writeValid,
             |  data_vd,
             |  data_offset,
             |  data_mask,
             |  data_data,
             |  data_instructionIndex,
             |  targetLane
             |);
             |endmodule
             |""".stripMargin
        )
      })
      peekWriteQueue.mshrIdx := i.U
      peekWriteQueue.clock := clock
      peekWriteQueue.data := enq_data
      peekWriteQueue.writeValid := tap(mshr.io.enq.valid)
      peekWriteQueue.targetLane := targetLane
  }

  dut.laneVec.zipWithIndex.foreach {
    case (lane, i) =>
      val writePeek = Module(new ExtModule with HasExtModuleInline {
        override val desiredName = "dpiPeekVrfWrite"
        val clock = IO(Input(Clock()))
        val valid = IO(Input(Bool()))
        val landIdx = IO(Input(UInt(32.W)))
        val request = IO(Input(new VRFWriteRequest(dut.parameter.vrfParam)))
        setInline(
          s"$desiredName.sv",
          s"""module $desiredName(
             |  input clock,
             |  input bit valid,
             |  input int landIdx,
             |  input bit[${request.vd.getWidth - 1}:0] request_vd,
             |  input bit[${request.offset.getWidth - 1}:0] request_offset,
             |  input bit[${request.mask.getWidth - 1}:0] request_mask,
             |  input bit[${request.data.getWidth - 1}:0] request_data,
             |  input bit[${request.instructionIndex.getWidth - 1}:0] request_instructionIndex,
             |  input bit[${request.last.getWidth - 1}:0] request_last
             |);
             |import "DPI-C" function void $desiredName(
             |  input int land_idx,
             |  input bit valid,
             |  input bit[${request.vd.getWidth - 1}:0] request_vd,
             |  input bit[${request.offset.getWidth - 1}:0] request_offset,
             |  input bit[${request.mask.getWidth - 1}:0] request_mask,
             |  input bit[${request.data.getWidth - 1}:0] request_data,
             |  input bit[${request.instructionIndex.getWidth - 1}:0] request_instructionIndex
             |);
             |always @ (posedge clock) #($latPeekVrfWrite) $desiredName(landIdx, valid, request_vd, request_offset, request_mask, request_data, request_instructionIndex);
             |endmodule
             |""".stripMargin
        )
      })
      writePeek.landIdx := i.U
      writePeek.valid := tap(lane.vrf.write.valid)
      writePeek.clock := clock
      writePeek.request := tap(lane.vrf.write.bits)
  }

  val dpiPokeInst = Module(new ExtModule with HasExtModuleInline {
    override val desiredName = "dpiPokeInst"
    val clock = IO(Input(Clock()))
    val request = IO(Output(new VRequest(dut.parameter.xLen)))
    val csrInterface = IO(Output(dut.csrInterface.cloneType))
    val instructionValid = IO(Output(Bool()))

    val respValid = IO(Input(Bool()))
    val response = IO(Input(dut.response.bits.cloneType))
    setInline(
      s"$desiredName.sv",
      s"""module $desiredName(
         |  input clock,
         |  output [31:0] request_instruction,
         |  output [${dut.parameter.xLen - 1}:0] request_src1Data,
         |  output [${dut.parameter.xLen - 1}:0] request_src2Data,
         |  output instructionValid,
         |
         |  output bit[${csrInterface.vl.getWidth - 1}:0] csrInterface_vl,
         |  output bit[${csrInterface.vStart.getWidth - 1}:0] csrInterface_vStart,
         |  output bit[${csrInterface.vlmul.getWidth - 1}:0] csrInterface_vlmul,
         |  output bit[${csrInterface.vSew.getWidth - 1}:0] csrInterface_vSew,
         |  output bit[${csrInterface.vxrm.getWidth - 1}:0] csrInterface_vxrm,
         |  output bit csrInterface_vta,
         |  output bit csrInterface_vma,
         |  output bit csrInterface_ignoreException,
         |
         |  input respValid,
         |  input bit[${response.getWidth - 1}:0] response_data
         |);
         |import "DPI-C" function void $desiredName(
         |  output bit[31:0] request_instruction,
         |  output bit[${dut.parameter.xLen - 1}:0] request_src1Data,
         |  output bit[${dut.parameter.xLen - 1}:0] request_src2Data,
         |  output bit instructionValid,
         |
         |  output bit[${csrInterface.vl.getWidth - 1}:0] vl,
         |  output bit[${csrInterface.vStart.getWidth - 1}:0] vStart,
         |  output bit[${csrInterface.vlmul.getWidth - 1}:0] vlmul,
         |  output bit[${csrInterface.vSew.getWidth - 1}:0] vSew,
         |  output bit[${csrInterface.vxrm.getWidth - 1}:0] vxrm,
         |  output bit vta,
         |  output bit vma,
         |  output bit ignoreException,
         |
         |  input respValid,
         |  input bit[${response.getWidth - 1}:0] response_data
         |);
         |always @ (posedge clock) #($latPokeInst) $desiredName(
         |  request_instruction,
         |  request_src1Data,
         |  request_src2Data,
         |  instructionValid,
         |
         |  csrInterface_vl,
         |  csrInterface_vStart,
         |  csrInterface_vlmul,
         |  csrInterface_vSew,
         |  csrInterface_vxrm,
         |  csrInterface_vta,
         |  csrInterface_vma,
         |  csrInterface_ignoreException,
         |
         |  respValid,
         |  response_data
         |);
         |endmodule
         |""".stripMargin
    )
  })
  dpiPokeInst.clock := clock
  dpiPokeInst.respValid := resp.valid
  dpiPokeInst.response := resp.bits
  csrInterface := dpiPokeInst.csrInterface
  req.bits := dpiPokeInst.request
  req.valid := dpiPokeInst.instructionValid

  val dpiPeekIssue = Module(new ExtModule with HasExtModuleInline {
    override val desiredName = "dpiPeekIssue"
    val clock = IO(Input(Clock()))
    val ready = IO(Input(Bool()))
    val issueIdx = IO(Input(UInt(dut.parameter.instructionIndexWidth.W)))
    setInline(
      s"$desiredName.sv",
      s"""module $desiredName(
         |  input clock,
         |  input ready,
         |  input bit[${issueIdx.getWidth - 1}:0] issueIdx
         |);
         |import "DPI-C" function void $desiredName(
         |  input ready,
         |  input bit[${issueIdx.getWidth - 1}:0] issueIdx
         |);
         |always @ (posedge clock) #($latPeekIssue) $desiredName(
         |  ready,
         |  issueIdx
         |);
         |endmodule
         |""".stripMargin
    )
  })
  dpiPeekIssue.clock := clock
  dpiPeekIssue.ready := req.ready
  dpiPeekIssue.issueIdx := tap(dut.instructionCounter)

  @instantiable
  class PeekTL(bundle: TLBundle) extends ExtModule with HasExtModuleInline {
    override val desiredName = "dpiPeekTL"
    @public val clock = IO(Input(Clock()))
    @public val channel = IO(Input(UInt(32.W)))
    @public val aBits:  TLChannelA = IO(Input(bundle.a.bits))
    @public val aValid: Bool = IO(Input(bundle.a.valid))
    @public val dReady: Bool = IO(Input(bundle.d.ready))
    setInline(
      "dpiPeekTL.sv",
      s"""module $desiredName(
         |  input clock,
         |  input int channel,
         |  input bit[${aBits.opcode.getWidth - 1}:0] aBits_opcode,
         |  input bit[${aBits.param.getWidth - 1}:0] aBits_param,
         |  input bit[${aBits.size.getWidth - 1}:0] aBits_size,
         |  input bit[${aBits.source.getWidth - 1}:0] aBits_source,
         |  input bit[${aBits.address.getWidth - 1}:0] aBits_address,
         |  input bit[${aBits.mask.getWidth - 1}:0] aBits_mask,
         |  input bit[${aBits.data.getWidth - 1}:0] aBits_data,
         |  input bit aBits_corrupt,
         |  input bit aValid,
         |  input bit dReady
         |);
         |import "DPI-C" function void $desiredName(
         |  input int channel_id,
         |  input bit[${aBits.opcode.getWidth - 1}:0] a_opcode,
         |  input bit[${aBits.param.getWidth - 1}:0] a_param,
         |  input bit[${aBits.size.getWidth - 1}:0] a_size,
         |  input bit[${aBits.source.getWidth - 1}:0] a_source,
         |  input bit[${aBits.address.getWidth - 1}:0] a_address,
         |  input bit[${aBits.mask.getWidth - 1}:0] a_mask,
         |  input bit[${aBits.data.getWidth - 1}:0] a_data,
         |  input bit a_corrupt,
         |  input bit a_valid,
         |  input bit d_ready
         |);
         |always @ (posedge clock) #($latPeekTL) $desiredName(
         |  channel,
         |  aBits_opcode,
         |  aBits_param,
         |  aBits_size,
         |  aBits_source,
         |  aBits_address,
         |  aBits_mask,
         |  aBits_data,
         |  aBits_corrupt,
         |  aValid,
         |  dReady
         |);
         |endmodule
         |""".stripMargin
    )
  }

  @instantiable
  class PokeTL(bundle: TLBundle) extends ExtModule with HasExtModuleInline {
    override val desiredName = "dpiPokeTL"
    @public val clock = IO(Input(Clock()))
    @public val channel = IO(Input(UInt(32.W)))
    @public val dBits = IO(Output(bundle.d.bits))
    @public val dValid = IO(Output(bundle.d.valid))
    @public val aReady = IO(Output(bundle.a.ready))
    @public val dReady = IO(Input(bundle.d.ready))
    setInline(
      s"$desiredName.sv",
      s"""module $desiredName(
         |  input clock,
         |  input int channel,
         |  output bit[${dBits.opcode.getWidth - 1}:0] dBits_opcode,
         |  output bit[${dBits.param.getWidth - 1}:0] dBits_param,
         |  output bit[${dBits.size.getWidth - 1}:0] dBits_size,
         |  output bit[${dBits.source.getWidth - 1}:0] dBits_source,
         |  output bit[${dBits.sink.getWidth - 1}:0] dBits_sink,
         |  output bit[${dBits.denied.getWidth - 1}:0] dBits_denied,
         |  output bit[${dBits.data.getWidth - 1}:0] dBits_data,
         |  output bit dBits_corrupt,
         |  output bit dValid,
         |  output bit aReady,
         |  input bit dReady
         |);
         |import "DPI-C" function void $desiredName(
         |  input int channel_id,
         |  output bit[${dBits.opcode.getWidth - 1}:0] d_opcode,
         |  output bit[${dBits.param.getWidth - 1}:0] d_param,
         |  output bit[${dBits.size.getWidth - 1}:0] d_size,
         |  output bit[${dBits.source.getWidth - 1}:0] d_source,
         |  output bit[${dBits.sink.getWidth - 1}:0] d_sink,
         |  output bit[${dBits.denied.getWidth - 1}:0] d_denied,
         |  output bit[${dBits.data.getWidth - 1}:0] d_data,
         |  output bit d_corrupt,
         |  output bit d_valid,
         |  output bit a_ready,
         |  input bit d_ready
         |);
         |always @ (posedge clock) #($latPokeTL) $desiredName(
         |  channel,
         |  dBits_opcode,
         |  dBits_param,
         |  dBits_size,
         |  dBits_source,
         |  dBits_sink,
         |  dBits_denied,
         |  dBits_data,
         |  dBits_corrupt,
         |  dValid,
         |  aReady,
         |  dReady
         |);
         |endmodule
         |""".stripMargin
    )
  }

  val peekTlDef = Definition(new PeekTL(dut.parameter.tlParam.bundle()))
  val pokeTlDef = Definition(new PokeTL(dut.parameter.tlParam.bundle()))
  tlPort.zipWithIndex.foreach {
    case (bundle, idx) =>
      val peek = Instance(peekTlDef)
      peek.clock := clock
      peek.channel := idx.U
      peek.aBits := bundle.a.bits
      peek.aValid := bundle.a.valid
      peek.dReady := bundle.d.ready

      val poke = Instance(pokeTlDef)
      poke.clock := clock
      poke.channel := idx.U
      bundle.d.bits := poke.dBits
      bundle.d.valid := poke.dValid
      poke.dReady := bundle.d.ready
      bundle.a.ready := poke.aReady
  }

  done()
}
