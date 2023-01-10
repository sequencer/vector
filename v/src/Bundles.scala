package v

import chisel3._
import chisel3.util.experimental.decode.DecodeBundle
import chisel3.util.{Decoupled, DecoupledIO, Valid, ValidIO, log2Ceil}
class VRequest(xLen: Int) extends Bundle {
  val instruction: UInt = UInt(32.W)
  val src1Data:    UInt = UInt(xLen.W)
  val src2Data:    UInt = UInt(xLen.W)
}

class VResponse(xLen: Int) extends Bundle {
  // todo: vector解出来是否需要写rd？
  val data: UInt = UInt(xLen.W)
}

class InstructionRecord(instructionIndexWidth: Int) extends Bundle {
  val instructionIndex: UInt = UInt(instructionIndexWidth.W)
  val vrfWrite:         Bool = Bool()

  /** Whether operation is widen */
  val widen: Bool = Bool()

  /** Whether operation is narrowing */
  val narrowing: Bool = Bool()
  // load | store
  val loadStore: Bool = Bool()
}

class InstructionState extends Bundle {
  val wLast:    Bool = Bool()
  val idle:     Bool = Bool()
  val sExecute: Bool = Bool()
  val sCommit:  Bool = Bool()
}

// TODO: rename
class SpecialInstructionType extends Bundle {
  val red:      Bool = Bool()
  val compress: Bool = Bool()
  val viota:    Bool = Bool()
  // 其他的需要对齐的指令
  val other: Bool = Bool()
}

class InstructionControl(instIndexWidth: Int, laneSize: Int) extends Bundle {
  val record: InstructionRecord = new InstructionRecord(instIndexWidth)
  val state:  InstructionState = new InstructionState

  /** tag for recording each lane and lsu is finished for this instruction. */
  val endTag: Vec[Bool] = Vec(laneSize + 1, Bool())
}

class ExtendInstructionType extends Bundle {
  val extend:   Bool = Bool()
  val mv:       Bool = Bool()
  val ffo:      Bool = Bool()
  val popCount: Bool = Bool()
  val id:       Bool = Bool()
}

class LaneRequest(param: LaneParameter) extends Bundle {
  val instructionIndex: UInt = UInt(param.instructionIndexSize.W)
  // decode
  val decodeResult: DecodeBundle = Decoder.bundle
  val loadStore:    Bool = Bool()
  val store:        Bool = Bool()
  val special:      Bool = Bool()

  // instruction
  /** vs1 or imm */
  val vs1: UInt = UInt(5.W)

  /** vs2 or rs2 */
  val vs2: UInt = UInt(5.W)

  /** vd or rd */
  val vd: UInt = UInt(5.W)

  val loadStoreEEW: UInt = UInt(2.W)

  /** mask type ? */
  val mask: Bool = Bool()

  val segment: UInt = UInt(3.W)

  /** data of rs1 */
  val readFromScalar: UInt = UInt(param.datapathWidth.W)

  // TODO: move to [[V]]
  def ma: Bool = decodeResult(Decoder.multiplier) && decodeResult(Decoder.uop)(1, 0).orR

  // TODO: move to Module
  def initState: InstGroupState = {
    val res: InstGroupState = Wire(new InstGroupState(param))
    val crossRead = decodeResult(Decoder.firstWiden) || decodeResult(Decoder.narrow)
    res.sRead1 := !decodeResult(Decoder.vtype)
    res.sRead2 := false.B
    res.sReadVD := !(crossRead || ma)
    res.wRead1 := !crossRead
    res.wRead2 := !crossRead
    res.wScheduler := !special
    res.sExecute := false.B
    //todo: red
    res.wExecuteRes := special
    res.sWrite := (decodeResult(Decoder.other) && decodeResult(Decoder.targetRd)) || decodeResult(
      Decoder.widen
    )
    res.sCrossWrite0 := !decodeResult(Decoder.widen)
    res.sCrossWrite1 := !decodeResult(Decoder.widen)
    res.sSendResult0 := !crossRead
    res.sSendResult1 := !crossRead
    res
  }

  // TODO: move to Module
  def instType: UInt = {
    VecInit(
      Seq(
        decodeResult(Decoder.logic) && !decodeResult(Decoder.other),
        decodeResult(Decoder.adder) && !decodeResult(Decoder.other),
        decodeResult(Decoder.shift) && !decodeResult(Decoder.other),
        decodeResult(Decoder.multiplier) && !decodeResult(Decoder.other),
        decodeResult(Decoder.divider) && !decodeResult(Decoder.other),
        decodeResult(Decoder.other)
      )
    ).asUInt
  }
}

class InstGroupState(param: LaneParameter) extends Bundle {
  // s for scheduler
  //   0 is for need to do but not do for now
  //   1 is done or don't need to do
  // w for wait
  //   0 is for need to wait and still waiting for finishing
  //   1 is for don't need to wait or already finished
  val sRead1:     Bool = Bool()
  val sRead2:     Bool = Bool()
  val sReadVD:    Bool = Bool()
  val wRead1:     Bool = Bool()
  val wRead2:     Bool = Bool()
  val wScheduler: Bool = Bool()
  val sExecute:   Bool = Bool()
  // 发送写的
  val sCrossWrite0: Bool = Bool()
  val sCrossWrite1: Bool = Bool()
  // 发送读的
  val sSendResult0: Bool = Bool()
  val sSendResult1: Bool = Bool()
  val wExecuteRes:  Bool = Bool()
  val sWrite:       Bool = Bool()
}

class InstControlRecord(param: LaneParameter) extends Bundle {
  val originalInformation: LaneRequest = new LaneRequest(param)
  val state:               InstGroupState = new InstGroupState(param)
  val initState:           InstGroupState = new InstGroupState(param)

  /** which group in the slot is executing. */
  val groupCounter:      UInt = UInt(param.groupNumberWidth.W)
  val schedulerComplete: Bool = Bool()

  /** the execution index in group
    * use byte as granularity,
    * SEW
    * 0 -> 00 01 10 11
    * 1 -> 00    10
    * 2 -> 00
    * TODO: 3(64 only)
    */
  val executeIndex: UInt = UInt(log2Ceil(param.dataPathByteWidth).W)

  /** 应对vl很小的时候,不会用到这条lane */
  val instCompleted: Bool = Bool()

  /** 存 mask */
  val mask: ValidIO[UInt] = Valid(UInt(param.datapathWidth.W))

  /** 把mask按每四个分一个组,然后看orR */
  val maskGroupedOrR: UInt = UInt((param.datapathWidth / param.sewMin).W)

  /** 这一组写vrf的mask */
  val vrfWriteMask: UInt = UInt(4.W)
}

class LaneCsrInterface(vlWidth: Int) extends Bundle {
  val vl:     UInt = UInt(vlWidth.W)
  val vStart: UInt = UInt(vlWidth.W)
  val vlmul:  UInt = UInt(3.W)
  val vSew:   UInt = UInt(2.W)

  /** Rounding mode register */
  val vxrm: UInt = UInt(2.W)

  /** tail agnostic */
  val vta: Bool = Bool()

  /** mask agnostic */
  val vma: Bool = Bool()

  /** 如果不忽视异常, fault only first 类型的指令需要等第一个回应 */
  val ignoreException: Bool = Bool()
}

class LaneDataResponse(param: LaneParameter) extends Bundle {
  val data: UInt = UInt(param.datapathWidth.W)
  // TODO: move to top?
  val toLSU:            Bool = Bool()
  val instructionIndex: UInt = UInt(param.instructionIndexSize.W)
}

class ReadBusData(param: LaneParameter) extends Bundle {
  val data: UInt = UInt(param.datapathWidth.W)
  val tail: Bool = Bool()
  // todo: for debug
  val from:      UInt = UInt(param.laneNumberWidth.W)
  val target:    UInt = UInt(param.laneNumberWidth.W)
  val instIndex: UInt = UInt(param.instructionIndexSize.W)
  val counter:   UInt = UInt(param.groupNumberWidth.W)
}

class WriteBusData(param: LaneParameter) extends Bundle {
  val data: UInt = UInt(param.datapathWidth.W)
  val tail: Bool = Bool()
  // 正常的跨lane写可能会有mask类型的指令
  val mask:   UInt = UInt(2.W)
  val target: UInt = UInt(param.laneNumberWidth.W)
  // todo: for debug
  val from:      UInt = UInt(param.laneNumberWidth.W)
  val instIndex: UInt = UInt(param.instructionIndexSize.W)
  val counter:   UInt = UInt(param.groupNumberWidth.W)
}

class RingPort[T <: Data](gen: T) extends Bundle {
  val enq: DecoupledIO[T] = Flipped(Decoupled(gen))
  val deq: DecoupledIO[T] = Decoupled(gen)
}

class SchedulerFeedback(param: LaneParameter) extends Bundle {
  val instructionIndex: UInt = UInt(param.instructionIndexSize.W)

  /** for instructions that might finish in other lanes, use [[complete]] to tell the target lane */
  val complete: Bool = Bool()
}

class V0Update(param: LaneParameter) extends Bundle {
  val data:   UInt = UInt(param.datapathWidth.W)
  val offset: UInt = UInt(param.vrfOffsetWidth.W)
  // mask/ld类型的有可能不会写完整的32bit
  val mask: UInt = UInt(4.W)
}

class VRFReadRequest(regNumBits: Int, offsetBits: Int, instructionIndexSize: Int) extends Bundle {
  // 为了方便处理seg类型的ld st, vs需要是明确的地址, 而不是一个base
  val vs: UInt = UInt(regNumBits.W)
  // 访问寄存器的 offset, 代表第几个32bit
  val offset: UInt = UInt(offsetBits.W)
  // 用来阻塞 raw
  val instructionIndex: UInt = UInt(instructionIndexSize.W)
}

class VRFWriteRequest(regNumBits: Int, offsetBits: Int, instructionIndexSize: Int, dataPathWidth: Int) extends Bundle {
  val vd:     UInt = UInt(regNumBits.W)
  val offset: UInt = UInt(offsetBits.W)
  // mask/ld类型的有可能不会写完整的32bit
  val mask:             UInt = UInt(4.W)
  val data:             UInt = UInt(dataPathWidth.W)
  val last:             Bool = Bool()
  val instructionIndex: UInt = UInt(instructionIndexSize.W)
}

class VRFWriteReport(param: VRFParam) extends Bundle {
  val vd:        ValidIO[UInt] = Valid(UInt(param.regNumBits.W))
  val vs1:       ValidIO[UInt] = Valid(UInt(param.regNumBits.W))
  val vs2:       UInt = UInt(param.regNumBits.W)
  val instIndex: UInt = UInt(param.instructionIndexSize.W)
  val vdOffset:  UInt = UInt(3.W)
  val offset:    UInt = UInt(param.offsetBits.W)
  val seg:       ValidIO[UInt] = Valid(UInt(3.W))
  val eew:       UInt = UInt(2.W)
  val ls:        Bool = Bool()
  val st:        Bool = Bool()
  val narrow:    Bool = Bool()
  val widen:     Bool = Bool()
  val stFinish:  Bool = Bool()
  // 乘加
  val ma:           Bool = Bool()
  val unOrderWrite: Bool = Bool()
}
