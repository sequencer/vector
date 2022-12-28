package v

import chisel3._
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.util._
import chisel3.util.experimental.decode.{decoder, TruthTable}
import tilelink.{TLBundle, TLBundleParameter, TLChannelAParameter, TLChannelDParameter}

object VParameter {
  implicit def rwP: upickle.default.ReadWriter[VParameter] = upickle.default.macroRW
}

/**
  * @param xLen XLEN
  * @param vLen VLEN
  * @param dataPathWidth width of data path, can be 32 or 64
  * @param laneNumer how many lanes in the vector processor
  * @param physicalAddressWidth width of memory bus address width
  * @param chainingSize how many instructions can be chained
  */
case class VParameter(
  xLen:                 Int,
  vLen:                 Int,
  dataPathWidth:        Int,
  laneNumer:            Int,
  physicalAddressWidth: Int,
  chainingSize:         Int,
  vrfWriteQueueSize:    Int)
    extends SerializableModuleParameter {
  val memoryBankSize: Int = 2

  /** TODO: uarch docs for mask(v0) group and normal vrf groups.
    *
    * The state machine in LSU.mshr will handle `dataPathWidth` bits data for each group.
    * For each lane, it will cache corresponding data in `dataPathWidth` bits.
    */
  val maskGroupWidth: Int = dataPathWidth

  /** how many groups will be divided into for mask(v0). */
  val maskGroupSize: Int = vLen / dataPathWidth

  /** TODO: uarch docs for instruction order maintenance.
    * 1 in MSB for instruction order.
    */
  val instructionIndexWidth: Int = log2Ceil(chainingSize) + 1

  /** maximum of lmul, defined in spec. */
  val lmulMax = 8

  /** Memory bundle parameter. */
  val memoryDataWidth: Int = dataPathWidth

  /** TODO: uarch docs
    *
    * width of tilelink source id
    * 5 for offset
    * 3 for segment index
    * 2 for 3 MSHR(2 read + 1 write)
    */
  val sourceWidth: Int = 10
  val sizeWidth:   Int = log2Ceil(log2Ceil(memoryDataWidth / 8)) + 1
  val maskWidth:   Int = memoryDataWidth / 8
  val tlParam: TLBundleParameter = TLBundleParameter(
    a = TLChannelAParameter(physicalAddressWidth, sourceWidth, memoryDataWidth, sizeWidth, maskWidth),
    b = None,
    c = None,
    d = TLChannelDParameter(sourceWidth, sourceWidth, memoryDataWidth, sizeWidth),
    e = None
  )
  // TODO: what a fuck are these parameters?
  def laneParam: LaneParameter =
    LaneParameter(
      vLen = vLen,
      datapathWidth = dataPathWidth,
      laneNumber = laneNumer,
      chainingSize = chainingSize,
      vrfWriteQueueSize = vrfWriteQueueSize
    )
  def lsuParam: LSUParam = LSUParam(dataPathWidth)
  def vrfParam: VRFParam = VRFParam(vLen, laneNumer, dataPathWidth)

  require(xLen == dataPathWidth)
}

class V(val parameter: VParameter) extends Module with SerializableModule[VParameter] {

  /** request from CPU.
    * it should come from commit stage.
    */
  val request: DecoupledIO[VRequest] = IO(Flipped(Decoupled(new VRequest(parameter.xLen))))

  /** response to CPU.
    * TODO: should be compatible to RoCC interface.
    */
  val response: ValidIO[VResponse] = IO(Valid(new VResponse(parameter.xLen)))

  /** CSR interface from CPU.
    */
  val csrInterface: LaneCsrInterface = IO(Input(new LaneCsrInterface(parameter.laneParam.vlWidth)))

  /** from CPU LSU, store buffer is cleared, memory can observe memory requests after this is asserted.
    */
  val storeBufferClear: Bool = IO(Input(Bool()))

  /** TileLink memory ports.
    */
  val memoryPorts: Vec[TLBundle] = IO(Vec(parameter.memoryBankSize, parameter.tlParam.bundle()))

  /** the LSU Module */
  val lsu: LSU = Module(new LSU(parameter.lsuParam))

  // TODO: cover overflow
  // TODO: uarch doc about the order of instructions
  val instructionCounter:     UInt = RegInit(0.U(parameter.instructionIndexWidth.W))
  val nextInstructionCounter: UInt = instructionCounter + 1.U
  when(request.fire) { instructionCounter := nextInstructionCounter }

  // todo: handle waw
  val responseCounter:     UInt = RegInit(0.U(parameter.instructionIndexWidth.W))
  val nextResponseCounter: UInt = responseCounter + 1.U
  when(response.fire) { responseCounter := nextResponseCounter }

  // 对进来的指令decode
  val decodeResult: UInt =
    decoder.espresso(
      (request.bits.instruction >> 12).asUInt,
      TruthTable(InstructionDecodeTable.table, BitPat.dontCare(25))
    )
  // TODO: split into two decoders
  val decodeResFormat:    InstructionDecodeResult = decodeResult.asTypeOf(new InstructionDecodeResult)
  val decodeResFormatExt: ExtendInstructionDecodeResult = decodeResult.asTypeOf(new ExtendInstructionDecodeResult)

  // TODO: these should be decoding results
  // TODO: no valid here
  val isLoadStoreType: Bool = !request.bits.instruction(6) && request.valid
  val isStoreType:     Bool = !request.bits.instruction(6) && request.bits.instruction(5)
  val maskType:        Bool = !request.bits.instruction(25)

  val noReadST:    Bool = isLoadStoreType && (!request.bits.instruction(26))
  val indexTypeLS: Bool = isLoadStoreType && request.bits.instruction(26)

  /** duplicate v0 for mask */
  val v0: Vec[UInt] = RegInit(VecInit(Seq.fill(parameter.maskGroupSize)(0.U(parameter.maskGroupWidth.W))))
  // TODO: if elen=32, vSew should be 2?
  val sew1H: UInt = UIntToOH(csrInterface.vSew)
  // TODO: uarch doc for the regroup
  val regroupV0: Seq[Vec[UInt]] = Seq(4, 2, 1).map { groupSize =>
    v0.map { element =>
      element.asBools
        .grouped(groupSize)
        .toSeq
        .map(VecInit(_).asUInt)
        .grouped(parameter.laneNumer)
        .toSeq
        .transpose
        .map(seq => VecInit(seq).asUInt)
    }.transpose.map(VecInit(_).asUInt)
  }.transpose.map(Mux1H(sew1H(2, 0), _)).map { v0ForLane =>
    VecInit(v0ForLane.asBools.grouped(32).toSeq.map(VecInit(_).asUInt))
  }

  /** which slot the instruction is entering */
  val instructionToSlotOH: UInt = Wire(UInt(parameter.chainingSize.W))

  /** synchronize signal from each lane, for mask units.(ffo) */
  val laneSynchronize: Vec[Bool] = Wire(Vec(parameter.laneNumer, Bool()))

  /** all lanes are synchronized. */
  val synchronized: Bool = Wire(Bool())

  /** last slot is committing. */
  val lastSlotCommit: Bool = Wire(Bool())
  // todo: special?
  val instructionType:     SpecialInstructionType = RegInit(0.U.asTypeOf(new SpecialInstructionType))
  val nextInstructionType: SpecialInstructionType = Wire(new SpecialInstructionType)

  /** for each lane, for instruction slot,
    * when asserted, the corresponding instruction is finished.
    */
  val instructionFinished: Vec[Vec[Bool]] = Wire(Vec(parameter.laneNumer, Vec(parameter.chainingSize, Bool())))

  // todo: no magic number, should be returned from decoder
  nextInstructionType.compress := decodeResFormat.otherUnit && decodeResFormat.uop === 5.U
  nextInstructionType.viota := decodeResFormat.otherUnit && decodeResFormat.uop(3) && decodeResFormatExt.viota
  nextInstructionType.red := !decodeResFormat.otherUnit && decodeResFormat.red
  // TODO: dont care?
  nextInstructionType.other := DontCare
  val maskUnitType: Bool = nextInstructionType.asUInt.orR
  // 是否在lane与schedule/lsu之间有数据交换,todo: decode
  val specialInst: Bool = maskUnitType || indexTypeLS
  val busClear:    Bool = Wire(Bool())

  /** state machine register for each instruction. */
  val instStateVec: Seq[InstructionControl] = Seq.tabulate(parameter.chainingSize) { index =>
    // todo: cover here
    val control = RegInit(
      (-1)
        .S(new InstructionControl(parameter.instructionIndexWidth, parameter.laneNumer).getWidth.W)
        .asTypeOf(new InstructionControl(parameter.instructionIndexWidth, parameter.laneNumer))
    )

    /** lsu is finished when report bits matched corresponding state machine */
    val lsuFinished: Bool = lsu.lastReport.valid && lsu.lastReport.bits === control.record.instructionIndex
    // instruction fire when instruction index matched corresponding state machine
    when(request.fire && instructionToSlotOH(index)) {
      // instruction metadata
      control.record.instructionIndex := instructionCounter
      // TODO: remove
      control.record.loadStore := isLoadStoreType
      // control signals
      control.state.idle := false.B
      control.state.wLast := false.B
      control.state.sCommit := false.B
      // two different initial states for endTag:
      // for load/store instruction, use the last bit to indicate whether it is the last instruction
      // for other instructions, use MSB to indicate whether it is the last instruction
      control.endTag := VecInit(Seq.fill(parameter.laneNumer)(isLoadStoreType) :+ !isLoadStoreType)
    }.otherwise {
      // TODO: remove wLast. last = control.endTag.asUInt.andR
      when(control.endTag.asUInt.andR) {
        control.state.wLast := !control.record.widen || busClear
      }
      // TODO: execute first, then commit
      when(responseCounter === control.record.instructionIndex && response.fire) {
        control.state.sCommit := true.B
      }
      when(control.state.sCommit && control.state.sExecute) {
        control.state.idle := true.B
      }

      // endTag update logic
      control.endTag.zip(instructionFinished.map(_(index)) :+ lsuFinished).foreach {
        case (d, c) => d := d || c
      }
    }
    // logic like mask&reduce will be put to last slot
    // TODO: review later
    if (index == (parameter.chainingSize - 1)) {
      val feedBack: UInt = RegInit(0.U(parameter.laneNumer.W))
      when(request.fire && instructionToSlotOH(index)) {
        control.state.sExecute := !maskType
        instructionType := nextInstructionType
      }.elsewhen(lastSlotCommit && control.state.wLast) {
        control.state.sExecute := true.B
      }
      when(laneSynchronize.asUInt.orR) {
        feedBack := feedBack | laneSynchronize.asUInt
      }.elsewhen(lastSlotCommit) {
        feedBack := 0.U
      }
      synchronized := feedBack.andR
    }
    control
  }

  /** data that need to be compute at top. */
  // TODO: dataSizeWith
  val data: Vec[ValidIO[UInt]] = RegInit(
    VecInit(Seq.fill(parameter.laneNumer)(0.U.asTypeOf(Valid(UInt(parameter.laneNumer.W)))))
  )
  val dataResult: ValidIO[UInt] = RegInit(0.U.asTypeOf(Valid(UInt(parameter.dataPathWidth.W))))
  // todo: viota & compress & reduce

  // TODO: remove
  val scheduleReady: Bool = VecInit(instStateVec.map(_.state.idle)).asUInt.orR
  // lane is ready to receive new instruction
  val laneReady:    Vec[Bool] = Wire(Vec(parameter.laneNumer, Bool()))
  val allLaneReady: Bool = laneReady.asUInt.andR
  // TODO: review later
  // todo: 把scheduler的反馈也加上,lsu有更高的优先级
  val laneFeedBackValid: Bool = lsu.lsuOffsetReq
  // todo:同样要加上scheduler的
  val laneComplete: Bool = lsu.lastReport.valid && lsu.lastReport.bits === instStateVec.last.record.instructionIndex

  val vrfWrite: Vec[DecoupledIO[VRFWriteRequest]] = Wire(
    Vec(parameter.laneNumer, Decoupled(new VRFWriteRequest(parameter.vrfParam)))
  )

  /** instantiate lanes.
    * TODO: move instantiate to top of class.
    */
  val laneVec: Seq[Lane] = Seq.tabulate(parameter.laneNumer) { index =>
    val lane: Lane = Module(new Lane(parameter.laneParam))
    // 请求,
    lane.laneRequest.valid := request.fire && !noReadST && allLaneReady
    lane.laneRequest.bits.instructionIndex := instructionCounter
    lane.laneRequest.bits.decodeResult := decodeResult
    lane.laneRequest.bits.vs1 := request.bits.instruction(19, 15)
    lane.laneRequest.bits.vs2 := request.bits.instruction(24, 20)
    lane.laneRequest.bits.vd := request.bits.instruction(11, 7)
    lane.laneRequest.bits.readFromScalar := request.bits.src1Data
    lane.laneRequest.bits.loadStore := isLoadStoreType
    lane.laneRequest.bits.store := isStoreType
    lane.laneRequest.bits.special := specialInst
    lane.laneRequest.bits.segment := request.bits.instruction(31, 29)
    lane.laneRequest.bits.loadStoreEEW := request.bits.instruction(13, 12)
    lane.laneRequest.bits.mask := maskType
//    lane.laneReq.bits.st := isST
    laneReady(index) := lane.laneRequest.ready

    lane.csrInterface := csrInterface
    lane.laneIndex := index.U

    lane.laneResponseFeedback.valid := laneFeedBackValid
    lane.laneResponseFeedback.bits.complete := laneComplete
    lane.laneResponseFeedback.bits.instructionIndex := instStateVec.last.record.instructionIndex

    lane.vrfReadAddressChannel <> lsu.readDataPorts(index)
    lsu.readResults(index) := lane.vrfReadDataChannel
    lane.vrfWriteChannel <> vrfWrite(index)

    lsu.offsetReadResult(index).valid := lane.laneResponse.valid && lane.laneResponse.bits.toLSU
    lsu.offsetReadResult(index).bits := lane.laneResponse.bits.data
    lsu.offsetReadTag(index) := lane.laneResponse.bits.instructionIndex

    instructionFinished(index).zip(instStateVec.map(_.record.instructionIndex)).foreach {
      case (d, f) => d := (UIntToOH(f(parameter.instructionIndexWidth - 2, 0)) & lane.instructionFinished).orR
    }
    lane.maskInput := regroupV0(index)(lane.maskSelect)
    lane.lsuLastReport := lsu.lastReport
    lane.lsuVRFWriteBufferClear := !lsu.vrfWritePort(index).valid

    lane
  }
  busClear := !VecInit(laneVec.map(_.writeBusPort.deq.valid)).asUInt.orR
  laneVec.map(_.laneResponse).zip(laneSynchronize).foreach {
    case (source, sink) => sink := source.valid && !source.bits.toLSU
  }

  // 连lsu
  lsu.req.valid := request.fire && isLoadStoreType
  lsu.req.bits.instIndex := instructionCounter
  lsu.req.bits.rs1Data := request.bits.src1Data
  lsu.req.bits.rs2Data := request.bits.src2Data
  lsu.req.bits.instInf.nf := request.bits.instruction(31, 29)
  lsu.req.bits.instInf.mew := request.bits.instruction(28)
  lsu.req.bits.instInf.mop := request.bits.instruction(27, 26)
  lsu.req.bits.instInf.vs1 := request.bits.instruction(19, 15)
  lsu.req.bits.instInf.vs2 := request.bits.instruction(24, 20)
  lsu.req.bits.instInf.vs3 := request.bits.instruction(11, 7)
  // (0b000 0b101 0b110 0b111) -> (8, 16, 32, 64)忽略最高位
  lsu.req.bits.instInf.eew := request.bits.instruction(13, 12)
  lsu.req.bits.instInf.st := isStoreType
  lsu.req.bits.instInf.mask := maskType

  lsu.maskRegInput.zip(lsu.maskSelect).foreach { case (data, index) => data := v0(index) }
  lsu.csrInterface := csrInterface

  // 连lane的环
  laneVec.map(_.readBusPort).foldLeft(laneVec.last.readBusPort) {
    case (previous, current) =>
      current.enq <> previous.deq
      current
  }
  laneVec.map(_.writeBusPort).foldLeft(laneVec.last.writeBusPort) {
    case (previous, current) =>
      current.enq <> previous.deq
      current
  }

  // 连 tilelink
  memoryPorts.zip(lsu.tlPort).foreach {
    case (source, sink) =>
      val dBuffer = Queue(source.d, 1, flow = true)
      sink <> source
      sink.d <> dBuffer
  }
  // 暂时直接连lsu的写,后续需要处理scheduler的写
  vrfWrite.zip(lsu.vrfWritePort).foreach { case (sink, source) => sink <> source }

  // instruction issue
  {
    val free = VecInit(instStateVec.map(_.state.idle)).asUInt
    val freeOR = free.orR
    val free1H = ffo(free)
    // 类型信息：isLSType noReadLD specialInst
    val tryToEnq = Mux(specialInst, true.B ## 0.U((parameter.chainingSize - 1).W), free1H)
    // 有一个空闲的本地坑
    val localReady = Mux(specialInst, instStateVec.map(_.state.idle).last, freeOR)
    // 远程坑就绪
    val executionReady = (!isLoadStoreType || lsu.req.ready) && (noReadST || allLaneReady)
    request.ready := executionReady && localReady
    instructionToSlotOH := Mux(request.ready, tryToEnq, 0.U)
  }

  // instruction commit
  {
    val slotCommit: Vec[Bool] = VecInit(instStateVec.map { inst =>
      inst.state.sExecute && inst.state.wLast && !inst.state.sCommit && inst.record.instructionIndex === responseCounter
    })
    response.valid := slotCommit.asUInt.orR
    response.bits.data := dataResult.bits
    lastSlotCommit := slotCommit.last
  }

  // write v0(mask)
  v0.zipWithIndex.foreach {
    case (data, index) =>
      // 属于哪个lane
      val laneIndex: Int = index % parameter.laneNumer
      // 取出写的端口
      val v0Write = laneVec(laneIndex).v0Update
      // offset
      val offset: Int = index / parameter.laneNumer
      val maskExt = FillInterleaved(8, v0Write.bits.mask)
      when(v0Write.valid && v0Write.bits.offset === offset.U) {
        data := (data & (~maskExt).asUInt) | (maskExt & v0Write.bits.data)
      }
  }
}
