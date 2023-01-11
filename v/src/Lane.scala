package v

import chisel3._
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.util._
import chisel3.util.experimental.decode.DecodeBundle

import scala.annotation.unused

object LaneParameter {
  implicit def rwP: upickle.default.ReadWriter[LaneParameter] = upickle.default.macroRW
}
case class LaneParameter(vLen: Int, datapathWidth: Int, laneNumber: Int, chainingSize: Int, vrfWriteQueueSize: Int)
  extends SerializableModuleParameter {
  val instructionIndexSize: Int = log2Ceil(chainingSize) + 1
  val lmulMax:              Int = 8
  val sewMin:               Int = 8
  val dataPathByteWidth:    Int = datapathWidth / sewMin
  val vlMax:                Int = vLen * lmulMax / sewMin

  /** width of vl
    * `+1` is for lv being 0 to vlMax(not vlMax - 1).
    * we use less than for comparing, rather than less equal.
    */
  val vlWidth: Int = log2Ceil(vlMax) + 1

  /** how many group does a single register have.
    *
    * in each lane, for one vector register, it is divided into groups with size of [[datapathWidth]]
    */
  val singleGroupSize: Int = vLen / datapathWidth / laneNumber

  /** for each instruction, the maximum number of groups to execute. */
  val groupNumberMax: Int = singleGroupSize * lmulMax

  /** used as the LSB index of VRF access
    *
    * TODO: uarch doc the arrangement of VRF: {reg index, offset}
    *
    * for each number in table below, it represent a [[datapathWidth]]
    * lane0 | lane1 | ...                                   | lane8
    * offset0    0  |    1  |    2  |    3  |    4  |    5  |    6  |    7
    * offset1    8  |    9  |   10  |   11  |   12  |   13  |   14  |   15
    * offset2   16  |   17  |   18  |   19  |   20  |   21  |   22  |   23
    * offset3   24  |   25  |   26  |   27  |   28  |   29  |   30  |   31
    */
  val vrfOffsetWidth: Int = log2Ceil(singleGroupSize)

  /** +1 for comparing to next group number. */
  val groupNumberWidth: Int = log2Ceil(groupNumberMax) + 1
  // TODO: remove
  val HLEN: Int = datapathWidth / 2

  /** uarch TODO: instantiate logic, add to each slot
    * shift, multiple, divide, other
    *
    * TODO: use Seq().size to calculate
    */
  val executeUnitNum:     Int = 6
  val laneNumberWidth:    Int = log2Ceil(laneNumber)
  val datapathWidthWidth: Int = log2Ceil(datapathWidth)

  /** see [[VParameter.maskGroupWidth]] */
  val maskGroupWidth: Int = datapathWidth

  /** see [[VParameter.maskGroupSize]] */
  val maskGroupSize:      Int = vLen / datapathWidth
  val maskGroupSizeWidth: Int = log2Ceil(maskGroupSize)

  def vrfParam:         VRFParam = VRFParam(vLen, laneNumber, datapathWidth, chainingSize, vrfWriteQueueSize)
  def datePathParam:    DataPathParam = DataPathParam(datapathWidth)
  def shifterParameter: LaneShifterParameter = LaneShifterParameter(datapathWidth, datapathWidthWidth)
  def mulParam:         LaneMulParam = LaneMulParam(datapathWidth)
  def indexParam:       LaneIndexCalculatorParameter = LaneIndexCalculatorParameter(groupNumberWidth, laneNumberWidth)
}

class Lane(val parameter: LaneParameter) extends Module with SerializableModule[LaneParameter] {

  /** laneIndex is a IO constant for D/I and physical implementations. */
  val laneIndex: UInt = IO(Input(UInt(parameter.laneNumberWidth.W)))
  dontTouch(laneIndex)

  /** VRF Read Interface.
    * TODO: use mesh
    */
  val readBusPort: RingPort[ReadBusData] = IO(new RingPort(new ReadBusData(parameter)))

  /** VRF Write Interface.
    * TODO: use mesh
    */
  val writeBusPort: RingPort[WriteBusData] = IO(new RingPort(new WriteBusData(parameter)))

  /** request from [[V]] to [[Lane]] */
  val laneRequest: DecoupledIO[LaneRequest] = IO(Flipped(Decoupled(new LaneRequest(parameter))))

  /** CSR Interface. */
  val csrInterface: LaneCsrInterface = IO(Input(new LaneCsrInterface(parameter.vlWidth)))

  /** to mask unit or LSU */
  val laneResponse: ValidIO[LaneDataResponse] = IO(Valid(new LaneDataResponse(parameter)))

  /** feedback from [[V]] for [[laneResponse]] */
  val laneResponseFeedback: ValidIO[SchedulerFeedback] = IO(Flipped(Valid(new SchedulerFeedback(parameter))))

  // for LSU and V accessing lane, this is not a part of ring, but a direct connection.
  // TODO: learn AXI channel, reuse [[vrfReadAddressChannel]] and [[vrfWriteChannel]].
  val vrfReadAddressChannel: DecoupledIO[VRFReadRequest] = IO(
    Flipped(
      Decoupled(
        new VRFReadRequest(parameter.vrfParam.regNumBits, parameter.vrfOffsetWidth, parameter.instructionIndexSize)
      )
    )
  )
  val vrfReadDataChannel: UInt = IO(Output(UInt(parameter.datapathWidth.W)))
  val vrfWriteChannel: DecoupledIO[VRFWriteRequest] = IO(
    Flipped(
      Decoupled(
        new VRFWriteRequest(
          parameter.vrfParam.regNumBits,
          parameter.vrfOffsetWidth,
          parameter.instructionIndexSize,
          parameter.datapathWidth
        )
      )
    )
  )

  /** for each instruction in the slot, response to top when instruction is finished in this lane. */
  val instructionFinished: UInt = IO(Output(UInt(parameter.chainingSize.W)))

  /** V0 update in the lane should also update [[V.v0]] */
  val v0Update: ValidIO[V0Update] = IO(Valid(new V0Update(parameter)))

  /** input of mask data */
  val maskInput: UInt = IO(Input(UInt(parameter.maskGroupWidth.W)))

  /** select which mask group. */
  val maskSelect: UInt = IO(Output(UInt(parameter.maskGroupSizeWidth.W)))

  /** because of load store index EEW, is complicated for lane to calculate whether LSU is finished.
    * let LSU directly tell each lane it is finished.
    */
  val lsuLastReport: ValidIO[UInt] = IO(Flipped(Valid(UInt(parameter.instructionIndexSize.W))))

  /** for RaW, VRF should wait for buffer to be empty. */
  val lsuVRFWriteBufferClear: Bool = IO(Input(Bool()))

  // TODO: remove
  dontTouch(writeBusPort)

  /** VRF instantces. */
  val vrf: VRF = Module(new VRF(parameter.vrfParam))

  /** TODO: review later
    */
  val maskGroupedOrR: UInt = VecInit(
    maskInput.asBools
      .grouped(parameter.dataPathByteWidth)
      .toSeq
      .map(
        VecInit(_).asUInt.orR
      )
  ).asUInt

  /** the slot is occupied by instruction */
  val slotOccupied: Vec[Bool] = RegInit(VecInit(Seq.fill(parameter.chainingSize)(false.B)))

  /** read from VRF vs1 for VFU */
  val source1: Vec[UInt] = RegInit(VecInit(Seq.fill(parameter.chainingSize)(0.U(parameter.datapathWidth.W))))

  /** read from VRF vs2 for VFU */
  val source2: Vec[UInt] = RegInit(VecInit(Seq.fill(parameter.chainingSize)(0.U(parameter.datapathWidth.W))))

  /** read from VRF rd for VFU */
  val source3: Vec[UInt] = RegInit(VecInit(Seq.fill(parameter.chainingSize)(0.U(parameter.datapathWidth.W))))

  /** execution result, write to VRF,
    * or goes to [[V]] for complex instructions like reduce
    */
  val result: Vec[UInt] = RegInit(VecInit(Seq.fill(parameter.chainingSize)(0.U(parameter.datapathWidth.W))))
  // 跨lane写的额外用寄存器存储执行的结果和mask

  // wait data for EEW = 2*SEW
  // TODO: do we need to switch to remote waiting?
  val crossWriteResultLSBHalf: UInt = RegInit(0.U(parameter.datapathWidth.W))
  val crossWriteMaskLSBHalf:   UInt = RegInit(0.U((parameter.dataPathByteWidth / 2).W))
  val crossWriteResultMSBHalf: UInt = RegInit(0.U(parameter.datapathWidth.W))
  val crossWriteMaskMSBHalf:   UInt = RegInit(0.U((parameter.dataPathByteWidth / 2).W))

  /** arbiter for VRF write
    * 1 for [[vrfWriteChannel]]
    */
  val vrfWriteArbiter: Vec[ValidIO[VRFWriteRequest]] = Wire(
    Vec(
      parameter.chainingSize + 1,
      Valid(
        new VRFWriteRequest(
          parameter.vrfParam.regNumBits,
          parameter.vrfOffsetWidth,
          parameter.instructionIndexSize,
          parameter.datapathWidth
        )
      )
    )
  )
  vrfWriteArbiter(parameter.chainingSize).valid := vrfWriteChannel.valid
  vrfWriteArbiter(parameter.chainingSize).bits := vrfWriteChannel.bits

  /** writing to VRF
    * 1 for [[vrfWriteChannel]]
    * 1 for [[crossLaneWriteQueue]]
    */
  val vrfWriteFire: UInt = Wire(UInt((parameter.chainingSize + 2).W))
  vrfWriteChannel.ready := vrfWriteFire(parameter.chainingSize)

  /** for each slot, assert when it is asking [[V]] to change mask */
  val slotMaskRequestVec: Vec[ValidIO[UInt]] = Wire(
    Vec(
      parameter.chainingSize,
      Valid(UInt(parameter.maskGroupSizeWidth.W))
    )
  )

  /** which slot wins the arbitration for requesting mask. */
  val maskRequestFireOH: UInt = Wire(UInt(parameter.chainingSize.W))

  /** read from VRF, it will go to ring in the next cycle.
    * from [[vrfReadRequest]](1)
    */
  val crossReadLSBOut: UInt = RegInit(0.U(parameter.datapathWidth.W))

  /** read from VRF, it will go to ring in the next cycle.
    * from [[vrfReadRequest]](2)
    */
  val crossReadMSBOut: UInt = RegInit(0.U(parameter.datapathWidth.W))

  /** read from Bus, it will try to write to VRF in the next cycle. */
  val crossReadLSBIn: UInt = RegInit(0.U(parameter.datapathWidth.W))

  /** read from Bus, it will try to write to VRF in the next cycle. */
  val crossReadMSBIn: UInt = RegInit(0.U(parameter.datapathWidth.W))

  /** FSM control for each slot. */
  val slotControl: Vec[InstControlRecord] =
    RegInit(
      VecInit(
        Seq.fill(parameter.chainingSize)(0.U.asTypeOf(new InstControlRecord(parameter)))
      )
    )

  /** VRF read request for each slot,
    * 3 is for [[source1]] [[source2]] [[source3]]
    */
  val vrfReadRequest: Vec[Vec[DecoupledIO[VRFReadRequest]]] = Wire(
    Vec(
      parameter.chainingSize,
      Vec(
        3,
        Decoupled(
          new VRFReadRequest(parameter.vrfParam.regNumBits, parameter.vrfOffsetWidth, parameter.instructionIndexSize)
        )
      )
    )
  )

  /** VRF read result for each slot,
    * 3 is for [[source1]] [[source2]] [[source3]]
    */
  val vrfReadResult: Vec[Vec[UInt]] = Wire(
    Vec(
      parameter.chainingSize,
      Vec(3, UInt(parameter.datapathWidth.W))
    )
  )

  /** signal used for prohibiting slots to access VRF.
    * a slot will become inactive when:
    * 1. cross lane read/write is not finished
    * 2. lanes doesn't win mask request
    */
  val slotActive: Vec[Bool] = Wire(Vec(parameter.chainingSize, Bool()))

  /** TODO: uarch doc how to shift slot
    * the slot can slot shift when:
    */
  val slotCanShift: Vec[Bool] = Wire(Vec(parameter.chainingSize, Bool()))

  /** cross lane reading port from [[readBusPort]]
    * if [[ReadBusData.target]] matches the index of this lane, dequeue from ring
    */
  val readBusDequeue: ValidIO[ReadBusData] = Wire(Valid(new ReadBusData(parameter: LaneParameter)))

  // control signals for VFU, see [[parameter.executeUnitNum]]
  /** enqueue valid for execution unit */
  val executeEnqueueValid: Vec[UInt] = Wire(Vec(parameter.chainingSize, UInt(parameter.executeUnitNum.W)))

  /** enqueue fire signal for execution unit */
  val executeEnqueueFire: UInt = Wire(UInt(parameter.executeUnitNum.W))

  /** for most of time, dequeue is enqueue,
    * for long latency FPU(divider), fire signal is from that unit
    */
  val executeDequeueFire: UInt = Wire(UInt(parameter.executeUnitNum.W))

  /** execution result for each VFU. */
  val executeDequeueData: Vec[UInt] = Wire(Vec(parameter.executeUnitNum, UInt(parameter.datapathWidth.W)))

  /** for each slot, it occupies which VFU. */
  val instructionTypeVec: Vec[UInt] = Wire(Vec(parameter.chainingSize, UInt(parameter.executeUnitNum.W)))

  /** a instruction is finished in this lane. */
  val instructionExecuteFinished: Vec[Bool] = Wire(Vec(parameter.chainingSize, Bool()))

  /** Instructions that read across lane will have an extra set of reads if vl is not aligned. */
  val instructionCrossReadFinished: Bool = Wire(Bool())

  /** valid for requesting mask unit. */
  val maskRequestValids: Vec[Bool] = Wire(Vec(parameter.chainingSize, Bool()))

  /** request for logic instruction type. */
  val logicRequests: Vec[LaneLogicRequest] = Wire(
    Vec(parameter.chainingSize, new LaneLogicRequest(parameter.datePathParam))
  )

  /** request for adder instruction type. */
  val adderRequests: Vec[LaneAdderReq] = Wire(Vec(parameter.chainingSize, new LaneAdderReq(parameter.datePathParam)))

  /** request for shift instruction type. */
  val shiftRequests: Vec[LaneShifterReq] = Wire(
    Vec(parameter.chainingSize, new LaneShifterReq(parameter.shifterParameter))
  )

  /** request for multipler instruction type. */
  val multiplerRequests: Vec[LaneMulReq] = Wire(Vec(parameter.chainingSize, new LaneMulReq(parameter.mulParam)))

  /** request for divider instruction type. */
  val dividerRequests: Vec[LaneDivRequest] = Wire(
    Vec(parameter.chainingSize, new LaneDivRequest(parameter.datePathParam))
  )

  /** request for other instruction type. */
  val otherRequests: Vec[OtherUnitReq] = Wire(Vec(parameter.chainingSize, Output(new OtherUnitReq(parameter))))

  /** request for mask instruction type. */
  val maskRequests: Vec[LaneDataResponse] = Wire(Vec(parameter.chainingSize, Output(new LaneDataResponse(parameter))))

  /** assert when a instruction is finished in the slot. */
  val instructionFinishedVec: Vec[UInt] = Wire(Vec(parameter.chainingSize, UInt(parameter.chainingSize.W)))

  /** ready signal for enqueuing [[readBusPort]] */
  val crossLaneReadReady: Bool = Wire(Bool())

  /** ready signal for enqueuing [[writeBusPort]] */
  val crossLaneWriteReady: Bool = Wire(Bool())

  /** data for enqueuing [[readBusPort]]
    * [[crossLaneRead.valid]] indicate there is a slot try to enqueue [[readBusPort]]
    */
  val crossLaneRead: ValidIO[ReadBusData] = Wire(Valid(new ReadBusData(parameter)))

  /** data for enqueuing [[writeBusPort]]
    * [[crossLaneWrite.valid]] indicate there is a slot try to enqueue [[writeBusPort]]
    */
  val crossLaneWrite: ValidIO[WriteBusData] = Wire(Valid(new WriteBusData(parameter)))

  val vSewOrR: Bool = csrInterface.vSew.orR
  val vSew1H:  UInt = UIntToOH(csrInterface.vSew)
  // todo: - 1 in [[v]]
  val lastElementIndex: UInt = (csrInterface.vl - 1.U)(parameter.vlWidth - 2, 0)

  /** For an instruction, the last group is not executed by all lanes,
    * here is the last group of the instruction
    * xxxxx xxx xx -> vsew = 0
    * xxxxxx xxx x -> vsew = 1
    * xxxxxxx xxx  -> vsew = 2
    */
  val lastGroupForInstruction: UInt = Mux1H(
    vSew1H(2, 0),
    Seq(
      lastElementIndex(parameter.vlWidth - 2, parameter.laneNumberWidth + 2),
      lastElementIndex(parameter.vlWidth - 2, parameter.laneNumberWidth + 1),
      lastElementIndex(parameter.vlWidth - 2, parameter.laneNumberWidth)
    )
  )

  /** Which lane the last element is in. */
  val lastLaneIndex: UInt = Mux1H(
    vSew1H(2, 0),
    Seq(
      lastElementIndex(parameter.laneNumberWidth + 2 - 1, 2),
      lastElementIndex(parameter.laneNumberWidth + 1 - 1, 1),
      lastElementIndex(parameter.laneNumberWidth - 1, 0)
    )
  )

  /** Used to calculate the last group of mask, which will only be effective when [[isEndLane]] */
  val lastElementExecuteIndex: UInt = Mux1H(
    vSew1H(1, 0),
    Seq(
      lastElementIndex(1, 0),
      lastElementIndex(0) ## false.B
    )
  )

  /** The relative position of the last lane determines the processing of the last group. */
  val lanePositionLargerThanEndLane: Bool = laneIndex > lastLaneIndex
  val isEndLane:                     Bool = laneIndex === lastLaneIndex
  val lastGroupForLane:              UInt = lastGroupForInstruction - lanePositionLargerThanEndLane

  /** when [[InstControlRecord.executeIndex]] reaches [[slotGroupFinishedIndex]], the group in the slot is finished.
    * 00 -> 11
    * 01 -> 10
    * 10 -> 00
    *
    * TODO: 64bit
    */
  val slotGroupFinishedIndex: UInt = !csrInterface.vSew(1) ## !vSewOrR
  val lastGroupSlotGroupFinishedIndex: UInt = Mux(
    isEndLane,
    lastElementExecuteIndex,
    slotGroupFinishedIndex
  )

  /** queue for cross lane writing. */
  val crossLaneWriteQueue: Queue[VRFWriteRequest] = Module(
    new Queue(
      new VRFWriteRequest(
        parameter.vrfParam.regNumBits,
        parameter.vrfOffsetWidth,
        parameter.instructionIndexSize,
        parameter.datapathWidth
      ),
      parameter.vrfWriteQueueSize
    )
  )

  slotControl.zipWithIndex.foreach {
    case (record, index) =>
      if (index != 0) {
        // read only
        val decodeResult: DecodeBundle = record.originalInformation.decodeResult
        // TODO: use decode
        val needCrossRead = decodeResult(Decoder.firstWiden) || decodeResult(Decoder.narrow)
        // TODO: use decode
        val needCrossWrite = decodeResult(Decoder.widen)

        /** select from VFU, send to [[result]], [[crossWriteResultLSBHalf]], [[crossWriteResultMSBHalf]]. */
        val dataDequeue: UInt = Mux1H(instructionTypeVec(index), executeDequeueData)

        /** fire of [[dataDequeue]] */
        val dataDequeueFire: Bool = (instructionTypeVec(index) & executeDequeueFire).orR

        /** [[record.groupCounter]] & [[record.executeIndex]] is used as index of current data sending to VFU.
          * By default, data is not masked. due the the logic in [[next1H]], it is not masked.
          * when updating [[record.mask.bits]], the pointer is updated to the first item of mask group.
          * but this element might be masked.
          * So firstMasked is used to take care of this.
          */
        val firstMasked: Bool = Wire(Bool())
        // TODO: move this to verification module
        when(needCrossRead) {
          assert(csrInterface.vSew != 2.U)
        }

        /** for non-masked instruction, always ready,
          * for masked instruction, need to wait for mask
          */
        val maskReady: Bool = record.mask.valid || !record.originalInformation.mask
        slotActive(index) :=
          // slot should alive
          slotOccupied(index) &&
            // head should alive, if not, the slot should shift to make head alive
            slotOccupied.head &&
            // cross lane instruction should execute in the first slot
            !(needCrossRead || needCrossWrite) &&
            // mask should ready for masked instruction
            maskReady

        // shift slot
        slotCanShift(index) := !record.state.sExecute

        // vs1 read
        vrfReadRequest(index)(0).valid := !record.state.sRead1 && slotActive(index)
        vrfReadRequest(index)(0).bits.offset := record.groupCounter(parameter.vrfOffsetWidth - 1, 0)
        // todo: when vlmul > 0 use ## rather than +
        vrfReadRequest(index)(0).bits.vs := record.originalInformation.vs1 + record.groupCounter(
          parameter.groupNumberWidth - 1,
          parameter.vrfOffsetWidth
        )
        // used for hazard detection
        vrfReadRequest(index)(0).bits.instructionIndex := record.originalInformation.instructionIndex

        // vs2 read
        vrfReadRequest(index)(1).valid := !record.state.sRead2 && slotActive(index)
        vrfReadRequest(index)(1).bits.offset := record.groupCounter(parameter.vrfOffsetWidth - 1, 0)
        // todo: when vlmul > 0 use ## rather than +
        // TODO: pull Mux to standalone signal
        vrfReadRequest(index)(1).bits.vs := record.originalInformation.vs2 +
          record.groupCounter(parameter.groupNumberWidth - 1, parameter.vrfOffsetWidth)
        vrfReadRequest(index)(1).bits.instructionIndex := record.originalInformation.instructionIndex

        // vd read
        vrfReadRequest(index)(2).valid := !record.state.sReadVD && slotActive(index)
        vrfReadRequest(index)(2).bits.offset := record.groupCounter(parameter.vrfOffsetWidth - 1, 0)
        vrfReadRequest(index)(2).bits.vs := record.originalInformation.vd +
          record.groupCounter(parameter.groupNumberWidth - 1, parameter.vrfOffsetWidth)
        // for hazard detection
        vrfReadRequest(index)(2).bits.instructionIndex := record.originalInformation.instructionIndex

        /** all read operation is finished. */
        val readFinish =
        // VRF read
          record.state.sReadVD &&
            record.state.sRead1 &&
            record.state.sRead2

        // state machine control
        when(vrfReadRequest(index)(0).fire) {
          record.state.sRead1 := true.B
          // todo: datapath Mux
          source1(index) := vrfReadResult(index)(0)
        }
        when(vrfReadRequest(index)(1).fire) {
          record.state.sRead2 := true.B
          source2(index) := vrfReadResult(index)(1)
        }
        when(vrfReadRequest(index)(2).fire) {
          record.state.sReadVD := true.B
          source3(index) := vrfReadResult(index)(2)
        }

        // 发起执行单元的请求
        /** 计算结果需要偏移的: executeIndex * 8 */
        val dataOffset: UInt = record.executeIndex ## 0.U(3.W)

        /** 正在算的是这个lane的第多少个 element */
        val elementIndex: UInt = Mux1H(
          vSew1H(2, 0),
          Seq(
            (record.groupCounter ## record.executeIndex)(4, 0),
            (record.groupCounter ## record.executeIndex(1))(4, 0),
            record.groupCounter
          )
        )

        firstMasked :=
          // is mask type
          record.originalInformation.mask &&
            // mask is valid
            record.mask.valid &&
            // is executing first element in mask group
            (elementIndex(4, 0) === 0.U) &&
            // this element is masked
            !record.mask.bits(0)
        // 选出下一个element的index
        val maskCorrection: UInt = Mux1H(
          Seq(record.originalInformation.mask && record.mask.valid, !record.originalInformation.mask),
          Seq(record.mask.bits, (-1.S(parameter.datapathWidth.W)).asUInt)
        )
        val next1H =
          ffo((scanLeftOr(UIntToOH(elementIndex(4, 0))) ## false.B) & maskCorrection)(parameter.datapathWidth - 1, 0)
        val nextOrR: Bool = next1H.orR
        // nextIndex.getWidth = 5
        val nextIndex: UInt = OHToUInt(next1H)

        /** 这一组的mask已经没有剩余了 */
        val maskNeedUpdate = !nextOrR
        val nextGroupCountMSB: UInt = Mux1H(
          vSew1H(1, 0),
          Seq(
            record.groupCounter(parameter.groupNumberWidth - 1, parameter.groupNumberWidth - 3),
            false.B ## record.groupCounter(parameter.groupNumberWidth - 1, parameter.groupNumberWidth - 2)
          )
        ) + maskNeedUpdate
        val indexInLane = nextGroupCountMSB ## nextIndex
        // csrInterface.vSew 只会取值0, 1, 2,需要特别处理移位
        val nextIntermediateVolume = (indexInLane << csrInterface.vSew).asUInt
        val nextGroupCount = nextIntermediateVolume(parameter.groupNumberWidth + 1, 2)
        val nextExecuteIndex = nextIntermediateVolume(1, 0)

        /** 虽然没有计算完一组,但是这一组剩余的都被mask去掉了 */
        val maskFilterEnd = record.originalInformation.mask && (nextGroupCount =/= record.groupCounter)

        /** 需要一个除vl导致的end来决定下一个的 element index 是什么 */
        val dataDepletion = record.executeIndex === slotGroupFinishedIndex || maskFilterEnd

        /** 这一组计算全完成了 */
        val groupEnd = dataDepletion || instructionExecuteFinished(index)

        /** 计算当前这一组的 vrf mask
          * 已知：mask mask1H executeIndex
          * sew match:
          * 0:
          * executeIndex match:
          * 0: 0001
          * 1: 0010
          * 2: 0100
          * 3: 1000
          * 1:
          * executeIndex(0) match:
          * 0: 0011
          * 1: 1100
          * 2:
          * 1111
          */
        val executeByteEnable = Mux1H(
          vSew1H(2, 0),
          Seq(
            UIntToOH(record.executeIndex),
            record.executeIndex(1) ## record.executeIndex(1) ## !record.executeIndex(1) ## !record.executeIndex(1),
            15.U(4.W)
          )
        )
        val executeBitEnable: UInt = FillInterleaved(8, executeByteEnable)

        def CollapseOperand(data: UInt, enable: Bool = true.B, sign: Bool = false.B): UInt = {
          val dataMasked: UInt = data & executeBitEnable
          val select:     UInt = Mux(enable, vSew1H(2, 0), 4.U(3.W))
          // when sew = 0
          val collapse0 = Seq.tabulate(4)(i => dataMasked(8 * i + 7, 8 * i)).reduce(_ | _)
          // when sew = 1
          val collapse1 = Seq.tabulate(2)(i => dataMasked(16 * i + 15, 16 * i)).reduce(_ | _)
          Mux1H(
            select,
            Seq(
              Fill(24, sign && collapse0(7)) ## collapse0,
              Fill(16, sign && collapse1(15)) ## collapse1,
              data
            )
          )
        }

        // 处理操作数
        /**
          * src1： src1有 IXV 三种类型,只有V类型的需要移位
          */
        val finalSource1 =
          CollapseOperand(source1(index), decodeResult(Decoder.vtype), !decodeResult(Decoder.unsigned0))

        /** source2 一定是V类型的 */
        val finalSource2 = CollapseOperand(source2(index), true.B, !decodeResult(Decoder.unsigned1))

        /** source3 有两种：adc & ma, c等处理mask的时候再处理 */
        val finalSource3 = CollapseOperand(source3(index))

        val nextElementIndex = Mux1H(
          vSew1H,
          Seq(
            indexInLane(indexInLane.getWidth - 1, 2) ## laneIndex ## indexInLane(1, 0),
            indexInLane(indexInLane.getWidth - 1, 1) ## laneIndex ## indexInLane(0),
            indexInLane ## laneIndex
          )
        )
        instructionExecuteFinished(index) := nextElementIndex >= csrInterface.vl
        // 假如这个单元执行的是logic的类型的,请求应该是什么样子的
        val logicRequest = Wire(new LaneLogicRequest(parameter.datePathParam))
        logicRequest.src.head := finalSource2
        logicRequest.src.last := finalSource1
        logicRequest.opcode := decodeResult(Decoder.uop)
        // 在手动做Mux1H
        logicRequests(index) := maskAnd(
          slotOccupied(index) && decodeResult(Decoder.logic) && !decodeResult(Decoder.other),
          logicRequest
        )

        // adder 的
        val adderRequest = Wire(new LaneAdderReq(parameter.datePathParam))
        adderRequest.src := VecInit(Seq(finalSource1, finalSource2, finalSource3))
        adderRequest.opcode := decodeResult(Decoder.uop)
        adderRequest.sign := !decodeResult(Decoder.unsigned1)
        adderRequest.reverse := decodeResult(Decoder.reverse)
        adderRequest.average := decodeResult(Decoder.average)
        adderRequests(index) := maskAnd(
          slotOccupied(index) && decodeResult(Decoder.adder) && !decodeResult(Decoder.other),
          adderRequest
        )

        // shift 的
        val shiftRequest = Wire(new LaneShifterReq(parameter.shifterParameter))
        shiftRequest.src := finalSource2
        shiftRequest.shifterSize := Mux1H(
          vSew1H(2, 1),
          Seq(false.B ## finalSource1(3), finalSource1(4, 3))
        ) ## finalSource1(2, 0)
        shiftRequest.opcode := decodeResult(Decoder.uop)
        shiftRequests(index) := maskAnd(
          slotOccupied(index) && decodeResult(Decoder.shift) && !decodeResult(Decoder.other),
          shiftRequest
        )

        // mul
        val mulRequest: LaneMulReq = Wire(new LaneMulReq(parameter.mulParam))
        mulRequest.src := VecInit(Seq(finalSource1, finalSource2, finalSource3))
        mulRequest.opcode := decodeResult(Decoder.uop)
        multiplerRequests(index) := maskAnd(
          slotOccupied(index) && decodeResult(Decoder.multiplier) && !decodeResult(Decoder.other),
          mulRequest
        )

        // div
        val divRequest = Wire(new LaneDivRequest(parameter.datePathParam))
        divRequest.src := VecInit(Seq(finalSource1, finalSource2))
        divRequest.rem := decodeResult(Decoder.uop)(0)
        divRequest.sign := decodeResult(Decoder.unsigned0)
        dividerRequests(index) := maskAnd(
          slotOccupied(index) && decodeResult(Decoder.divider) && !decodeResult(Decoder.other),
          divRequest
        )

        // other
        val otherRequest: OtherUnitReq = Wire(Output(new OtherUnitReq(parameter)))
        otherRequest.src := VecInit(Seq(finalSource1, finalSource2))
        otherRequest.opcode := decodeResult(Decoder.uop)(2, 0)
        otherRequest.imm := record.originalInformation.vs1
        otherRequest.extendType.valid := decodeResult(Decoder.uop)(3)
        otherRequest.extendType.bits.elements.foreach { case (s, d) => d := decodeResult.elements(s) }
        otherRequest.laneIndex := laneIndex
        otherRequest.groupIndex := record.groupCounter
        otherRequest.sign := !decodeResult(Decoder.unsigned0)
        otherRequests(index) := maskAnd(slotOccupied(index) && decodeResult(Decoder.other), otherRequest)

        // 往scheduler的执行任务compress viota
        val maskRequest: LaneDataResponse = Wire(Output(new LaneDataResponse(parameter)))

        // viota & compress & ls 需要给外边数据
        val maskType: Bool =
          (record.originalInformation.special || record.originalInformation.loadStore) && slotActive(index)
        val maskValid = maskType && readFinish && !record.state.sExecute
        // 往外边发的是原始的数据
        maskRequest.data := source2(index)
        maskRequest.toLSU := record.originalInformation.loadStore
        maskRequest.instructionIndex := record.originalInformation.instructionIndex
        maskRequests(index) := maskAnd(slotOccupied(index) && maskValid, maskRequest)
        maskRequestValids(index) := maskValid

        when(
          laneResponseFeedback.valid && laneResponseFeedback.bits.instructionIndex === record.originalInformation.instructionIndex
        ) {
          record.state.wScheduler := true.B
        }
        instructionTypeVec(index) := record.originalInformation.instType
        executeEnqueueValid(index) := maskAnd(readFinish && !record.state.sExecute, instructionTypeVec(index))
        when((instructionTypeVec(index) & executeEnqueueFire).orR || maskValid) {
          when(groupEnd || maskValid) {
            record.state.sExecute := true.B
          }.otherwise {
            record.executeIndex := nextExecuteIndex
          }
        }

        // todo: 暂时先这样,处理mask的时候需要修
        val executeResult = (dataDequeue << dataOffset).asUInt(parameter.datapathWidth - 1, 0)
        val resultUpdate: UInt = (executeResult & executeBitEnable) | (result(index) & (~executeBitEnable).asUInt)
        when(dataDequeueFire) {
          when(groupEnd) {
            record.state.wExecuteRes := true.B
          }
          result(index) := resultUpdate
          when(!firstMasked) {
            record.vrfWriteMask := record.vrfWriteMask | executeByteEnable
          }
        }
        // 写rf
        vrfWriteArbiter(index).valid := record.state.wExecuteRes && !record.state.sWrite && slotActive(index)
        vrfWriteArbiter(index).bits.vd := record.originalInformation.vd + record.groupCounter(
          parameter.groupNumberWidth - 1,
          parameter.vrfOffsetWidth
        )
        vrfWriteArbiter(index).bits.offset := record.groupCounter
        vrfWriteArbiter(index).bits.data := result(index)
        vrfWriteArbiter(index).bits.last := instructionExecuteFinished(index)
        vrfWriteArbiter(index).bits.instructionIndex := record.originalInformation.instructionIndex
        vrfWriteArbiter(index).bits.mask := record.vrfWriteMask
        when(vrfWriteFire(index)) {
          record.state.sWrite := true.B
        }
        instructionFinishedVec(index) := 0.U(parameter.chainingSize.W)
        val maskUnhindered = maskRequestFireOH(index) || !maskNeedUpdate
        when((record.state.asUInt.andR && maskUnhindered) || record.instCompleted) {
          when(instructionExecuteFinished(index) || record.instCompleted) {
            slotOccupied(index) := false.B
            when(slotOccupied(index)) {
              instructionFinishedVec(index) := UIntToOH(
                record.originalInformation.instructionIndex(parameter.instructionIndexSize - 2, 0)
              )
            }
          }.otherwise {
            record.state := record.initState
            record.groupCounter := nextGroupCount
            record.executeIndex := nextExecuteIndex
            record.vrfWriteMask := 0.U
            when(maskRequestFireOH(index)) {
              record.mask.valid := true.B
              record.mask.bits := maskInput
              record.maskGroupedOrR := maskGroupedOrR
            }
          }
        }
        when(
          laneResponseFeedback.bits.complete && laneResponseFeedback.bits.instructionIndex === record.originalInformation.instructionIndex
        ) {
          // 例如:别的lane找到了第一个1
          record.schedulerComplete := true.B
          when(record.originalInformation.special) {
            slotOccupied(index) := false.B
          }
        }
        // mask 更换
        slotMaskRequestVec(index).valid := maskNeedUpdate
        slotMaskRequestVec(index).bits := nextGroupCountMSB
      } else {
        // read only
        val decodeResult: DecodeBundle = record.originalInformation.decodeResult
        // TODO: use decode
        val needCrossRead = decodeResult(Decoder.firstWiden) || decodeResult(Decoder.narrow)

        /** Each group needs a state machine,
          * so we will ignore the effect of mask to jump to the next group at the end of this group
          * [[needCrossRead]]: We need to read data to another lane
          * [[record.originalInformation.special]]: We need to synchronize with [[V]] every group
          */
        val alwaysNextGroup: Bool = needCrossRead ||
          record.originalInformation.special

        /** select from VFU, send to [[result]], [[crossWriteResultLSBHalf]], [[crossWriteResultMSBHalf]]. */
        val dataDequeue: UInt = Mux1H(instructionTypeVec(index), executeDequeueData)

        /** fire of [[dataDequeue]] */
        val dataDequeueFire: Bool = (instructionTypeVec(index) & executeDequeueFire).orR

        /** 直接表示这个element被mask了 */
        val masked: Bool = Wire(Bool())
        // TODO: move this to verification module
        when(needCrossRead) {
          assert(csrInterface.vSew != 2.U)
        }

        /** for non-masked instruction, always ready,
          * for masked instruction, need to wait for mask
          */
        val maskReady: Bool = record.mask.valid || !record.originalInformation.mask
        slotActive(index) :=
          // slot should alive
          slotOccupied(index) &&
            // head should alive, if not, the slot should shift to make head alive
            slotOccupied.head &&
            // mask should ready for masked instruction
            maskReady

        // shift slot
        slotCanShift(index) := !record.state.sExecute

        // vs1 read
        vrfReadRequest(index)(0).valid := !record.state.sRead1 && slotActive(index)
        vrfReadRequest(index)(0).bits.offset := record.groupCounter(parameter.vrfOffsetWidth - 1, 0)
        // todo: when vlmul > 0 use ## rather than +
        vrfReadRequest(index)(0).bits.vs := record.originalInformation.vs1 + record.groupCounter(
          parameter.groupNumberWidth - 1,
          parameter.vrfOffsetWidth
        )
        // used for hazard detection
        vrfReadRequest(index)(0).bits.instructionIndex := record.originalInformation.instructionIndex

        // TODO: VRF uarch doc
        //
        // example:
        //  0 ->   0  |    1 ->   0  |    2 ->   1  |    3 ->   1  |    4 ->   2  |    5 ->   2  |    6 ->   3  |    7 ->   3
        //  8 ->   4  |    9 ->   4  |   10 ->   5  |   11 ->   5  |   12 ->   6  |   13 ->   6  |   14 ->   7  |   15 ->   7
        // 16 ->   8  |   17 ->   8  |   18 ->   9  |   19 ->   9  |   20 ->  10  |   21 ->  10  |   22 ->  11  |   23 ->  11
        // 24 ->  12  |   25 ->  12  |   26 ->  13  |   27 ->  13  |   28 ->  14  |   29 ->  14  |   30 ->  15  |   31 ->  15

        // vs2 read
        vrfReadRequest(index)(1).valid := !record.state.sRead2 && slotActive(index)
        vrfReadRequest(index)(1).bits.offset := Mux(
          needCrossRead,
          record.groupCounter(parameter.vrfOffsetWidth - 2, 0) ## false.B,
          record.groupCounter(parameter.vrfOffsetWidth - 1, 0)
        )
        // todo: when vlmul > 0 use ## rather than +
        // TODO: pull Mux to standalone signal
        vrfReadRequest(index)(1).bits.vs := record.originalInformation.vs2 + Mux(
          needCrossRead,
          record.groupCounter(parameter.groupNumberWidth - 2, parameter.vrfOffsetWidth - 1),
          record.groupCounter(parameter.groupNumberWidth - 1, parameter.vrfOffsetWidth)
        )
        vrfReadRequest(index)(1).bits.instructionIndex := record.originalInformation.instructionIndex

        // vd read
        vrfReadRequest(index)(2).valid := !record.state.sReadVD && slotActive(index)
        vrfReadRequest(index)(2).bits.offset := Mux(
          needCrossRead,
          record.groupCounter(parameter.vrfOffsetWidth - 2, 0) ## true.B,
          record.groupCounter(parameter.vrfOffsetWidth - 1, 0)
        )
        vrfReadRequest(index)(2).bits.vs := Mux(
          needCrossRead,
          // cross lane access use vs2
          record.originalInformation.vs2,
          // for MAC use vd
          record.originalInformation.vd
        ) +
          Mux(
            needCrossRead,
            record.groupCounter(parameter.groupNumberWidth - 2, parameter.vrfOffsetWidth - 1),
            record.groupCounter(parameter.groupNumberWidth - 1, parameter.vrfOffsetWidth)
          )
        // for hazard detection
        vrfReadRequest(index)(2).bits.instructionIndex := record.originalInformation.instructionIndex

        /** all read operation is finished. */
        val readFinish =
        // VRF read
          record.state.sReadVD &&
            record.state.sRead1 &&
            record.state.sRead2 &&
            // wait for cross lane read result
            record.state.wRead1 &&
            record.state.wRead2

        // state machine control
        when(vrfReadRequest(index)(0).fire) {
          record.state.sRead1 := true.B
          // todo: datapath Mux
          source1(index) := vrfReadResult(index)(0)
        }
        when(vrfReadRequest(index)(1).fire) {
          record.state.sRead2 := true.B
          source2(index) := vrfReadResult(index)(1)
        }
        when(vrfReadRequest(index)(2).fire) {
          record.state.sReadVD := true.B
          source3(index) := vrfReadResult(index)(2)
        }

        // cross lane read
        val tryToSendHead = record.state.sRead2 && !record.state.sSendResult0 && slotOccupied.head
        val tryToSendTail = record.state.sReadVD && !record.state.sSendResult1 && slotOccupied.head
        crossLaneRead.bits.target := (!tryToSendHead) ## laneIndex(parameter.laneNumberWidth - 1, 1)
        crossLaneRead.bits.tail := laneIndex(0)
        crossLaneRead.bits.from := laneIndex
        crossLaneRead.bits.instIndex := record.originalInformation.instructionIndex
        crossLaneRead.bits.counter := record.groupCounter
        crossLaneRead.bits.data := Mux(tryToSendHead, crossReadLSBOut, crossReadMSBOut)
        crossLaneRead.valid := tryToSendHead || tryToSendTail

        // 跨lane的写
        val sendWriteHead = record.state.sExecute && !record.state.sCrossWrite0 && slotOccupied.head
        val sendWriteTail = record.state.sExecute && !record.state.sCrossWrite1 && slotOccupied.head
        crossLaneWrite.bits.target := laneIndex(parameter.laneNumberWidth - 2, 0) ## (!sendWriteHead)
        crossLaneWrite.bits.from := laneIndex
        crossLaneWrite.bits.tail := laneIndex(parameter.laneNumberWidth - 1)
        crossLaneWrite.bits.instIndex := record.originalInformation.instructionIndex
        crossLaneWrite.bits.counter := record.groupCounter
        crossLaneWrite.bits.data := Mux(sendWriteHead, crossWriteResultLSBHalf, crossWriteResultMSBHalf)
        crossLaneWrite.bits.mask := Mux(sendWriteHead, crossWriteMaskLSBHalf, crossWriteMaskMSBHalf)
        crossLaneWrite.valid := sendWriteHead || sendWriteTail

        // 跨lane读写的数据接收
        when(readBusDequeue.valid) {
          assert(readBusDequeue.bits.instIndex === record.originalInformation.instructionIndex)
          when(readBusDequeue.bits.tail) {
            record.state.wRead2 := true.B
            crossReadMSBIn := readBusDequeue.bits.data
          }.otherwise {
            record.state.wRead1 := true.B
            crossReadLSBIn := readBusDequeue.bits.data
          }
        }

        // 读环发送的状态变化
        // todo: 处理发给自己的, 可以在使用的时候直接用读的寄存器, init state的时候自己纠正回来
        when(crossLaneReadReady && crossLaneRead.valid) {
          record.state.sSendResult0 := true.B
          when(record.state.sSendResult0) {
            record.state.sSendResult1 := true.B
          }
        }
        // 写环发送的状态变化
        when(crossLaneWriteReady && crossLaneWrite.valid) {
          record.state.sCrossWrite0 := true.B
          when(record.state.sCrossWrite0) {
            record.state.sCrossWrite1 := true.B
          }
        }

        // 跨lane的读记录
        when(vrfReadRequest(index)(1).fire && needCrossRead) {
          crossReadLSBOut := vrfReadResult(index)(1)
        }
        when(vrfReadRequest(index)(2).fire && needCrossRead) {
          crossReadMSBOut := vrfReadResult(index)(2)
        }

        /** 记录跨lane的写
          * sew = 2的时候不会有双倍的写,所以只需要处理sew=0和sew=1
          * sew:
          *   0:
          *     executeIndex:
          *       0: mask = 0011, head
          *       1: mask = 1100, head
          *       2: mask = 0011, tail
          *       3: mask = 1100, tail
          *   1:
          *     executeIndex:
          *       0: mask = 1111, head
          *       2: mask = 1111, tail
          */
        // dataDeq
        when(dataDequeueFire && !masked) {
          when(record.executeIndex(1)) {
            // update tail
            crossWriteResultMSBHalf :=
              Mux(
                csrInterface.vSew(0),
                dataDequeue(parameter.datapathWidth - 1, parameter.HLEN),
                Mux(
                  record.executeIndex(0),
                  dataDequeue(parameter.HLEN - 1, 0),
                  crossWriteResultMSBHalf(parameter.datapathWidth - 1, parameter.HLEN)
                )
              ) ## Mux(
                !record.executeIndex(0) || csrInterface.vSew(0),
                dataDequeue(parameter.HLEN - 1, 0),
                crossWriteResultMSBHalf(parameter.HLEN - 1, 0)
              )
            crossWriteMaskMSBHalf :=
              (record.executeIndex(0) || csrInterface.vSew(0) || crossWriteMaskMSBHalf(1)) ##
                (!record.executeIndex(0) || csrInterface.vSew(0) || crossWriteMaskMSBHalf(0))
          }.otherwise {
            // update head
            crossWriteResultLSBHalf :=
              Mux(
                csrInterface.vSew(0),
                dataDequeue(parameter.datapathWidth - 1, parameter.HLEN),
                Mux(
                  record.executeIndex(0),
                  dataDequeue(parameter.HLEN - 1, 0),
                  crossWriteResultLSBHalf(parameter.datapathWidth - 1, parameter.HLEN)
                )
              ) ## Mux(
                !record.executeIndex(0) || csrInterface.vSew(0),
                dataDequeue(parameter.HLEN - 1, 0),
                crossWriteResultLSBHalf(parameter.HLEN - 1, 0)
              )
            crossWriteMaskLSBHalf :=
              (record.executeIndex(0) || csrInterface.vSew(0) || crossWriteMaskLSBHalf(1)) ##
                (!record.executeIndex(0) || csrInterface.vSew(0) || crossWriteMaskLSBHalf(0))
          }

        }
        when(record.state.asUInt.andR) {
          crossWriteMaskLSBHalf := 0.U
          crossWriteMaskMSBHalf := 0.U
        }
        // 发起执行单元的请求
        /** 计算结果需要偏移的: executeIndex * 8 */
        val dataOffset: UInt = record.executeIndex ## 0.U(3.W)

        /** 正在算的是这个lane的第多少个 element */
        val elementIndex: UInt = Mux1H(
          vSew1H(2, 0),
          Seq(
            (record.groupCounter ## record.executeIndex)(4, 0),
            (record.groupCounter ## record.executeIndex(1))(4, 0),
            record.groupCounter
          )
        )

        masked :=
          // is mask type
          record.originalInformation.mask &&
            // mask is valid
            record.mask.valid &&
            !record.mask.bits(elementIndex(parameter.datapathWidthWidth - 1, 0))
        // 选出下一个element的index
        val maskCorrection: UInt = Mux1H(
          Seq(record.originalInformation.mask && record.mask.valid, !record.originalInformation.mask),
          Seq(record.mask.bits, (-1.S(parameter.datapathWidth.W)).asUInt)
        )
        val current1H = UIntToOH(elementIndex(4, 0))
        val next1H =
          ffo((scanLeftOr(current1H) ## false.B) & maskCorrection)(parameter.datapathWidth - 1, 0)
        val differentDataGroup: Bool = Mux1H(
          vSew1H(1, 0),
          Seq(4, 2).map { elementNumberPerGroup =>
            Seq(current1H, next1H).map { elementPosition =>
              VecInit(
                elementPosition.asBools
                  .grouped(elementNumberPerGroup)
                  .toSeq
                  .map(VecInit(_).asUInt.orR)
              ).asUInt
            }.reduce(_ & _).orR
          }
        ) || vSew1H(2)
        val nextOrR: Bool = next1H.orR
        // nextIndex.getWidth = 5
        val nextIndex: UInt = OHToUInt(next1H)
        /**
          * 一组mask有 [[parameter.maskGroupWidth]] 个bit
          * 每0组数据执行 4, 2, 1 个 element
          * */
        val lastGroupForMask = Mux1H(
          vSew1H(2, 0),
          Seq(
            record.groupCounter(log2Ceil(parameter.maskGroupWidth / 4) - 1, 0).andR,
            record.groupCounter(log2Ceil(parameter.maskGroupWidth / 2) - 1, 0).andR,
            record.groupCounter(log2Ceil(parameter.maskGroupWidth) - 1, 0).andR,
          )
        )

        /** 这一组的mask已经没有剩余了 */
        val maskNeedUpdate = !nextOrR && (!alwaysNextGroup || lastGroupForMask)
        val nextGroupCountMSB: UInt = Mux1H(
          vSew1H(1, 0),
          Seq(
            record.groupCounter(parameter.groupNumberWidth - 1, parameter.groupNumberWidth - 3),
            false.B ## record.groupCounter(parameter.groupNumberWidth - 1, parameter.groupNumberWidth - 2)
          )
        ) + maskNeedUpdate
        val indexInLane = nextGroupCountMSB ## nextIndex
        // csrInterface.vSew 只会取值0, 1, 2,需要特别处理移位
        val nextIntermediateVolume = (indexInLane << csrInterface.vSew).asUInt

        /** mask 后 ffo 计算出来的下一次计算的 element 属于哪一个 group */
        val nextGroupMasked: UInt = nextIntermediateVolume(parameter.groupNumberWidth + 1, 2)
        val nextGroupCount = Mux(
          alwaysNextGroup,
          record.groupCounter + 1.U,
          nextGroupMasked
        )

        /** 虽然没有计算完一组,但是这一组剩余的都被mask去掉了 */
        val maskFilterEnd = record.originalInformation.mask && (nextGroupMasked =/= record.groupCounter)

        /** 这是会执行的最后一组 */
        val lastExecuteGroup: Bool = lastGroupForLane === record.groupCounter

        /**
          * 需要一个除vl导致的end来决定下一个的 element index 是什么
          * 在vl没对齐的情况下，最后一组的结束的时候的 [[record.executeIndex]] 需要修正
          */
        val groupEnd = record.executeIndex === Mux(
          lastExecuteGroup,
          lastGroupSlotGroupFinishedIndex,
          slotGroupFinishedIndex
        ) || maskFilterEnd

        /** 只会发生在跨lane读 */
        val waitCrossRead = lastGroupForLane < record.groupCounter
        val lastVRFWrite: Bool = lastGroupForLane < nextGroupCount

        val nextExecuteIndex = Mux(
          alwaysNextGroup && groupEnd,
          0.U,
          nextIntermediateVolume(1, 0)
        )

        /** 计算当前这一组的 vrf mask
          * 已知：mask mask1H executeIndex
          * sew match:
          *   0:
          *     executeIndex match:
          *       0: 0001
          *       1: 0010
          *       2: 0100
          *       3: 1000
          *   1:
          *     executeIndex(0) match:
          *       0: 0011
          *       1: 1100
          *   2:
          *     1111
          */
        val executeByteEnable = Mux1H(
          vSew1H(2, 0),
          Seq(
            UIntToOH(record.executeIndex),
            record.executeIndex(1) ## record.executeIndex(1) ## !record.executeIndex(1) ## !record.executeIndex(1),
            15.U(4.W)
          )
        )
        val executeBitEnable: UInt = FillInterleaved(8, executeByteEnable)

        def CollapseOperand(data: UInt, enable: Bool = true.B, sign: Bool = false.B): UInt = {
          val dataMasked: UInt = data & executeBitEnable
          val select:     UInt = Mux(enable, vSew1H(2, 0), 4.U(3.W))
          // when sew = 0
          val collapse0 = Seq.tabulate(4)(i => dataMasked(8 * i + 7, 8 * i)).reduce(_ | _)
          // when sew = 1
          val collapse1 = Seq.tabulate(2)(i => dataMasked(16 * i + 15, 16 * i)).reduce(_ | _)
          Mux1H(
            select,
            Seq(
              Fill(24, sign && collapse0(7)) ## collapse0,
              Fill(16, sign && collapse1(15)) ## collapse1,
              data
            )
          )
        }

        // 有2 * sew 的操作数需要折叠
        def CollapseDoubleOperand(sign: Bool = false.B): UInt = {
          val doubleBitEnable = FillInterleaved(16, executeByteEnable)
          val doubleDataMasked: UInt = (crossReadMSBIn ## crossReadLSBIn) & doubleBitEnable
          val select:           UInt = vSew1H(1, 0)
          // when sew = 0
          val collapse0 = Seq.tabulate(4)(i => doubleDataMasked(16 * i + 15, 16 * i)).reduce(_ | _)
          // when sew = 1
          val collapse1 = Seq.tabulate(2)(i => doubleDataMasked(32 * i + 31, 32 * i)).reduce(_ | _)
          Mux1H(
            select,
            Seq(
              Fill(16, sign && collapse0(15)) ## collapse0,
              collapse1
            )
          )
        }
        // 处理操作数
        /**
          * src1： src1有 IXV 三种类型,只有V类型的需要移位
          */
        val finalSource1 =
          CollapseOperand(source1(index), decodeResult(Decoder.vtype), !decodeResult(Decoder.unsigned0))

        /** source2 一定是V类型的 */
        val doubleCollapse = CollapseDoubleOperand(!decodeResult(Decoder.unsigned1))
        val finalSource2 = Mux(
          needCrossRead,
          doubleCollapse,
          CollapseOperand(source2(index), true.B, !decodeResult(Decoder.unsigned1))
        )
        instructionExecuteFinished(index) := waitCrossRead ||
          (lastExecuteGroup && groupEnd)
        instructionCrossReadFinished := waitCrossRead && readFinish

        /** source3 有两种：adc & ma, c等处理mask的时候再处理 */
        val finalSource3 = CollapseOperand(source3(index))
        // 假如这个单元执行的是logic的类型的,请求应该是什么样子的
        val logicRequest = Wire(new LaneLogicRequest(parameter.datePathParam))
        logicRequest.src.head := finalSource2
        logicRequest.src.last := finalSource1
        logicRequest.opcode := decodeResult(Decoder.uop)
        // 在手动做Mux1H
        logicRequests(index) := maskAnd(
          slotOccupied(index) && decodeResult(Decoder.logic) && !decodeResult(Decoder.other),
          logicRequest
        )

        // adder 的
        val adderRequest = Wire(new LaneAdderReq(parameter.datePathParam))
        adderRequest.src := VecInit(Seq(finalSource1, finalSource2, finalSource3))
        adderRequest.opcode := decodeResult(Decoder.uop)
        adderRequest.sign := !decodeResult(Decoder.unsigned1)
        adderRequest.reverse := decodeResult(Decoder.reverse)
        adderRequest.average := decodeResult(Decoder.average)
        adderRequests(index) := maskAnd(
          slotOccupied(index) && decodeResult(Decoder.adder) && !decodeResult(Decoder.other),
          adderRequest
        )

        // shift 的
        val shiftRequest = Wire(new LaneShifterReq(parameter.shifterParameter))
        shiftRequest.src := finalSource2
        // 2 * sew 有额外1bit的
        shiftRequest.shifterSize := Mux1H(
          Mux(needCrossRead, vSew1H(1, 0), vSew1H(2, 1)),
          Seq(false.B ## finalSource1(3), finalSource1(4, 3))
        ) ## finalSource1(2, 0)
        shiftRequest.opcode := decodeResult(Decoder.uop)
        shiftRequests(index) := maskAnd(
          slotOccupied(index) && decodeResult(Decoder.shift) && !decodeResult(Decoder.other),
          shiftRequest
        )

        // mul
        val mulRequest: LaneMulReq = Wire(new LaneMulReq(parameter.mulParam))
        mulRequest.src := VecInit(Seq(finalSource1, finalSource2, finalSource3))
        mulRequest.opcode := decodeResult(Decoder.uop)
        multiplerRequests(index) := maskAnd(
          slotOccupied(index) && decodeResult(Decoder.multiplier) && !decodeResult(Decoder.other),
          mulRequest
        )

        // div
        val divRequest = Wire(new LaneDivRequest(parameter.datePathParam))
        divRequest.src := VecInit(Seq(finalSource1, finalSource2))
        divRequest.rem := decodeResult(Decoder.uop)(0)
        divRequest.sign := decodeResult(Decoder.unsigned0)
        dividerRequests(index) := maskAnd(
          slotOccupied(index) && decodeResult(Decoder.divider) && !decodeResult(Decoder.other),
          divRequest
        )

        // other
        val otherRequest: OtherUnitReq = Wire(Output(new OtherUnitReq(parameter)))
        otherRequest.src := VecInit(Seq(finalSource1, finalSource2))
        otherRequest.opcode := decodeResult(Decoder.uop)(2, 0)
        otherRequest.imm := record.originalInformation.vs1
        otherRequest.extendType.valid := decodeResult(Decoder.uop)(3)
        otherRequest.extendType.bits.elements.foreach { case (s, d) => d := decodeResult.elements(s) }
        otherRequest.laneIndex := laneIndex
        otherRequest.groupIndex := record.groupCounter
        otherRequest.sign := !decodeResult(Decoder.unsigned0)
        otherRequests(index) := maskAnd(slotOccupied(index) && decodeResult(Decoder.other), otherRequest)

        // 往scheduler的执行任务compress viota
        val maskRequest: LaneDataResponse = Wire(Output(new LaneDataResponse(parameter)))

        // viota & compress & ls 需要给外边数据
        val maskType: Bool =
          (record.originalInformation.special || record.originalInformation.loadStore) && slotActive(index)
        val maskValid = maskType && readFinish && !record.state.sExecute
        // 往外边发的是原始的数据
        maskRequest.data := source2(index)
        maskRequest.toLSU := record.originalInformation.loadStore
        maskRequest.instructionIndex := record.originalInformation.instructionIndex
        maskRequests(index) := maskAnd(slotOccupied(index) && maskValid, maskRequest)
        maskRequestValids(index) := maskValid

        when(
          laneResponseFeedback.valid && laneResponseFeedback.bits.instructionIndex === record.originalInformation.instructionIndex
        ) {
          record.state.wScheduler := true.B
        }
        instructionTypeVec(index) := record.originalInformation.instType
        executeEnqueueValid(index) := maskAnd(readFinish && !record.state.sExecute, instructionTypeVec(index))
        when((instructionTypeVec(index) & executeEnqueueFire).orR || maskValid) {
          when(groupEnd || maskValid) {
            record.state.sExecute := true.B
          }.otherwise {
            record.executeIndex := nextExecuteIndex
          }
        }

        // todo: 暂时先这样,处理mask的时候需要修
        val executeResult = (dataDequeue << dataOffset).asUInt(parameter.datapathWidth - 1, 0)
        val resultUpdate: UInt = (executeResult & executeBitEnable) | (result(index) & (~executeBitEnable).asUInt)
        when(dataDequeueFire) {
          when(groupEnd) {
            record.state.wExecuteRes := true.B
          }
          result(index) := resultUpdate
          when(!masked) {
            record.vrfWriteMask := record.vrfWriteMask | executeByteEnable
          }
        }
        // 写rf
        vrfWriteArbiter(index).valid := record.state.wExecuteRes && !record.state.sWrite && slotActive(index)
        vrfWriteArbiter(index).bits.vd := record.originalInformation.vd + record.groupCounter(
          parameter.groupNumberWidth - 1,
          parameter.vrfOffsetWidth
        )
        vrfWriteArbiter(index).bits.offset := record.groupCounter
        vrfWriteArbiter(index).bits.data := result(index)
        // todo: 是否条件有多余
        vrfWriteArbiter(index).bits.last := instructionExecuteFinished(index) || lastVRFWrite
        vrfWriteArbiter(index).bits.instructionIndex := record.originalInformation.instructionIndex
        vrfWriteArbiter(index).bits.mask := record.vrfWriteMask
        when(vrfWriteFire(index)) {
          record.state.sWrite := true.B
        }
        instructionFinishedVec(index) := 0.U(parameter.chainingSize.W)
        val maskUnhindered = maskRequestFireOH(index) || !maskNeedUpdate
        val stateCheck = Mux(
          waitCrossRead,
          readFinish,
          record.state.asUInt.andR && maskUnhindered
        )
        val crossReadReady: Bool = !needCrossRead || instructionCrossReadFinished
        when(stateCheck || record.instCompleted) {
          when((instructionExecuteFinished(index) && crossReadReady) || record.instCompleted) {
            slotOccupied(index) := false.B
            when(slotOccupied(index)) {
              instructionFinishedVec(index) := UIntToOH(
                record.originalInformation.instructionIndex(parameter.instructionIndexSize - 2, 0)
              )
            }
          }.otherwise {
            record.state := record.initState
            record.groupCounter := nextGroupCount
            record.executeIndex := nextExecuteIndex
            record.vrfWriteMask := 0.U
            when(maskRequestFireOH(index)) {
              record.mask.valid := true.B
              record.mask.bits := maskInput
              record.maskGroupedOrR := maskGroupedOrR
            }
          }
        }
        when(
          laneResponseFeedback.bits.complete && laneResponseFeedback.bits.instructionIndex === record.originalInformation.instructionIndex
        ) {
          // 例如:别的lane找到了第一个1
          record.schedulerComplete := true.B
          when(record.originalInformation.special) {
            slotOccupied(index) := false.B
          }
        }
        // mask 更换
        slotMaskRequestVec(index).valid := maskNeedUpdate
        slotMaskRequestVec(index).bits := nextGroupCountMSB
      }
  }

  // 处理读环的
  {
    val readBusDataReg: ValidIO[ReadBusData] = RegInit(0.U.asTypeOf(Valid(new ReadBusData(parameter))))
    val readBusDequeueMatch = readBusPort.enq.bits.target === laneIndex &&
      readBusPort.enq.bits.counter === slotControl.head.groupCounter
    readBusDequeue.valid := readBusDequeueMatch && readBusPort.enq.valid
    readBusDequeue.bits := readBusPort.enq.bits
    // 暂时优先级策略是环上的优先
    readBusPort.enq.ready := true.B
    readBusDataReg.valid := false.B

    when(readBusPort.enq.valid) {
      when(!readBusDequeueMatch) {
        readBusDataReg.valid := true.B
        readBusDataReg.bits := readBusPort.enq.bits
      }
    }

    // 试图进环
    readBusPort.deq.valid := readBusDataReg.valid || crossLaneRead.valid
    readBusPort.deq.bits := Mux(readBusDataReg.valid, readBusDataReg.bits, crossLaneRead.bits)
    crossLaneReadReady := !readBusDataReg.valid
  }

  // 处理写环
  {
    val writeBusDataReg: ValidIO[WriteBusData] = RegInit(0.U.asTypeOf(Valid(new WriteBusData(parameter))))
    // 策略依然是环上的优先,如果queue满了继续转
    val writeBusIndexMatch = writeBusPort.enq.bits.target === laneIndex && crossLaneWriteQueue.io.enq.ready
    writeBusPort.enq.ready := true.B
    writeBusDataReg.valid := false.B
    crossLaneWriteQueue.io.enq.bits.vd := slotControl.head.originalInformation.vd + writeBusPort.enq.bits.counter(3, 1)
    crossLaneWriteQueue.io.enq.bits.offset := writeBusPort.enq.bits.counter ## writeBusPort.enq.bits.tail
    crossLaneWriteQueue.io.enq.bits.data := writeBusPort.enq.bits.data
    crossLaneWriteQueue.io.enq.bits.last := instructionExecuteFinished.head && writeBusPort.enq.bits.tail
    crossLaneWriteQueue.io.enq.bits.instructionIndex := slotControl.head.originalInformation.instructionIndex
    crossLaneWriteQueue.io.enq.bits.mask := FillInterleaved(2, writeBusPort.enq.bits.mask)
    //writeBusPort.enq.bits
    crossLaneWriteQueue.io.enq.valid := false.B

    when(writeBusPort.enq.valid) {
      when(writeBusIndexMatch) {
        crossLaneWriteQueue.io.enq.valid := true.B
      }.otherwise {
        writeBusDataReg.valid := true.B
        writeBusDataReg.bits := writeBusPort.enq.bits
      }
    }

    // 进写环
    writeBusPort.deq.valid := writeBusDataReg.valid || crossLaneWrite.valid
    writeBusPort.deq.bits := Mux(writeBusDataReg.valid, writeBusDataReg.bits, crossLaneWrite.bits)
    crossLaneWriteReady := !writeBusDataReg.valid
  }

  // VFU
  // TODO: reuse logic, adder, multiplier datapath
  {
    val logicUnit: LaneLogic = Module(new LaneLogic(parameter.datePathParam))
    val adder:     LaneAdder = Module(new LaneAdder(parameter.datePathParam))
    val shifter:   LaneShifter = Module(new LaneShifter(parameter.shifterParameter))
    val mul:       LaneMul = Module(new LaneMul(parameter.mulParam))
    val div:       LaneDiv = Module(new LaneDiv(parameter.datePathParam))
    val otherUnit: OtherUnit = Module(new OtherUnit(parameter))

    // 连接执行单元的请求
    logicUnit.req := VecInit(logicRequests.map(_.asUInt))
      .reduce(_ | _)
      .asTypeOf(new LaneLogicRequest(parameter.datePathParam))
    adder.req := VecInit(adderRequests.map(_.asUInt)).reduce(_ | _).asTypeOf(new LaneAdderReq(parameter.datePathParam))
    shifter.req := VecInit(shiftRequests.map(_.asUInt))
      .reduce(_ | _)
      .asTypeOf(new LaneShifterReq(parameter.shifterParameter))
    mul.req := VecInit(multiplerRequests.map(_.asUInt)).reduce(_ | _).asTypeOf(new LaneMulReq(parameter.mulParam))
    div.req.bits := VecInit(dividerRequests.map(_.asUInt))
      .reduce(_ | _)
      .asTypeOf(new LaneDivRequest(parameter.datePathParam))
    otherUnit.req := VecInit(otherRequests.map(_.asUInt)).reduce(_ | _).asTypeOf(Output(new OtherUnitReq(parameter)))
    laneResponse.bits := VecInit(maskRequests.map(_.asUInt))
      .reduce(_ | _)
      .asTypeOf(Output(new LaneDataResponse(parameter)))
    laneResponse.valid := maskRequestValids.asUInt.orR
    // 执行单元的其他连接
    adder.csr.vSew := csrInterface.vSew
    adder.csr.vxrm := csrInterface.vxrm
    otherUnit.csr.vSew := csrInterface.vSew
    otherUnit.csr.vxrm := csrInterface.vxrm
    div.mask := DontCare
    div.vSew := csrInterface.vSew

    // 连接执行结果
    executeDequeueData := VecInit(
      Seq(logicUnit.resp, adder.resp, shifter.resp, mul.resp, div.resp.bits, otherUnit.resp.data)
    )
    executeDequeueFire := executeEnqueueFire(5) ## div.resp.valid ## executeEnqueueFire(3, 0)
    // 执行单元入口握手
    val tryToUseExecuteUnit = VecInit(executeEnqueueValid.map(_.asBools).transpose.map(VecInit(_).asUInt.orR)).asUInt
    executeEnqueueFire := tryToUseExecuteUnit & (true.B ## div.req.ready ## 15.U(4.W))
    div.req.valid := tryToUseExecuteUnit(4)
  }

  // 处理 rf
  {
    // 连接读口
    val readArbiter = Module(
      new Arbiter(
        new VRFReadRequest(parameter.vrfParam.regNumBits, parameter.vrfOffsetWidth, parameter.instructionIndexSize),
        8
      )
    )
    // 暂时把lsu的读放在了最低优先级,有问题再改
    (vrfReadRequest(1).last +: (vrfReadRequest(2) ++ vrfReadRequest(3)) :+ vrfReadAddressChannel)
      .zip(readArbiter.io.in)
      .foreach {
        case (source, sink) =>
          sink <> source
      }
    (vrfReadRequest.head ++ vrfReadRequest(1).init :+ readArbiter.io.out).zip(vrf.read).foreach {
      case (source, sink) =>
        sink <> source
    }

    // 读的结果
    vrfReadResult.foreach(a => a.foreach(_ := vrf.readResult.last))
    (vrfReadResult.head ++ vrfReadResult(1).init).zip(vrf.readResult.init).foreach {
      case (sink, source) =>
        sink := source
    }
    vrfReadDataChannel := vrf.readResult.last

    // 写 rf
    val normalWrite = VecInit(vrfWriteArbiter.map(_.valid)).asUInt.orR
    val writeSelect = !normalWrite ## ffo(VecInit(vrfWriteArbiter.map(_.valid)).asUInt)
    val writeEnqBits = Mux1H(writeSelect, vrfWriteArbiter.map(_.bits) :+ crossLaneWriteQueue.io.deq.bits)
    vrf.write.valid := normalWrite || crossLaneWriteQueue.io.deq.valid
    vrf.write.bits := writeEnqBits
    crossLaneWriteQueue.io.deq.ready := !normalWrite && vrf.write.ready
    vrfWriteFire := Mux(vrf.write.ready, writeSelect, 0.U)

    //更新v0
    v0Update.valid := vrf.write.valid && writeEnqBits.vd === 0.U
    v0Update.bits.data := writeEnqBits.data
    v0Update.bits.offset := writeEnqBits.offset
    v0Update.bits.mask := writeEnqBits.mask
  }

  {
    // 处理mask的请求
    val maskSelectArbitrator = ffo(
      VecInit(slotMaskRequestVec.map(_.valid)).asUInt ## (laneRequest.valid && laneRequest.bits.mask)
    )
    maskRequestFireOH := maskSelectArbitrator(parameter.chainingSize, 1)
    maskSelect := Mux1H(
      maskSelectArbitrator,
      0.U.asTypeOf(slotMaskRequestVec.head.bits) +: slotMaskRequestVec.map(_.bits)
    )
  }
  // 控制逻辑的移动
  val entranceControl: InstControlRecord = Wire(new InstControlRecord(parameter))
  entranceControl.originalInformation := laneRequest.bits
  entranceControl.state := laneRequest.bits.initState
  entranceControl.initState := laneRequest.bits.initState
  entranceControl.executeIndex := 0.U
  entranceControl.schedulerComplete := false.B
  entranceControl.instCompleted := ((laneIndex ## 0.U(2.W)) >> csrInterface.vSew).asUInt >= csrInterface.vl
  entranceControl.mask.valid := laneRequest.bits.mask
  entranceControl.mask.bits := maskInput
  entranceControl.maskGroupedOrR := maskGroupedOrR
  entranceControl.vrfWriteMask := 0.U
  // todo: vStart(2,0) > lane index
  entranceControl.groupCounter := (csrInterface.vStart >> 3).asUInt
  // todo: spec 10.1: imm 默认是 sign-extend,但是有特殊情况
  val vs1entrance: UInt =
    Mux(
      laneRequest.bits.decodeResult(Decoder.vtype),
      0.U,
      Mux(
        laneRequest.bits.decodeResult(Decoder.xtype),
        laneRequest.bits.readFromScalar,
        VecInit(Seq.fill(parameter.datapathWidth - 5)(laneRequest.bits.vs1(4))).asUInt ## laneRequest.bits.vs1
      )
    )
  val entranceInstType: UInt = laneRequest.bits.instType
  // todo: 修改v0的和使用v0作为mask的指令需要产生冲突
  val typeReady: Bool = VecInit(
    instructionTypeVec.zip(slotOccupied).map { case (t, v) => (t =/= entranceInstType) || !v }
  ).asUInt.andR
  val validRegulate: Bool = laneRequest.valid && typeReady
  laneRequest.ready := !slotOccupied.head && typeReady && vrf.instWriteReport.ready
  vrf.instWriteReport.valid := (laneRequest.fire || (!laneRequest.bits.store && laneRequest.bits.loadStore)) && !entranceControl.instCompleted
  when(!slotOccupied.head && (slotOccupied.asUInt.orR || validRegulate)) {
    slotOccupied := VecInit(slotOccupied.tail :+ validRegulate)
    source1 := VecInit(source1.tail :+ vs1entrance)
    slotControl := VecInit(slotControl.tail :+ entranceControl)
    result := VecInit(result.tail :+ 0.U(parameter.datapathWidth.W))
    source2 := VecInit(source2.tail :+ 0.U(parameter.datapathWidth.W))
    source3 := VecInit(source3.tail :+ 0.U(parameter.datapathWidth.W))
    crossWriteMaskLSBHalf := 0.U
    crossWriteMaskMSBHalf := 0.U
  }
  // 试图让vrf记录这一条指令的信息,拒绝了说明有还没解决的冲突
  vrf.flush := DontCare
  vrf.instWriteReport.bits.instIndex := laneRequest.bits.instructionIndex
  vrf.instWriteReport.bits.offset := 0.U //todo
  vrf.instWriteReport.bits.vdOffset := 0.U
  vrf.instWriteReport.bits.vd.bits := laneRequest.bits.vd
  vrf.instWriteReport.bits.vd.valid := !(laneRequest.bits.initState.sWrite || laneRequest.bits.store)
  vrf.instWriteReport.bits.vs2 := laneRequest.bits.vs2
  vrf.instWriteReport.bits.vs1.bits := laneRequest.bits.vs1
  vrf.instWriteReport.bits.vs1.valid := laneRequest.bits.decodeResult(Decoder.vtype)
  // TODO: move ma to [[V]]
  vrf.instWriteReport.bits.ma := laneRequest.bits.ma
  // 暂时认为ld都是无序写寄存器的
  vrf.instWriteReport.bits.unOrderWrite := (laneRequest.bits.loadStore && !laneRequest.bits.store) || laneRequest.bits
    .decodeResult(Decoder.other)
  vrf.instWriteReport.bits.seg.valid := laneRequest.bits.loadStore && laneRequest.bits.segment.orR
  vrf.instWriteReport.bits.seg.bits := laneRequest.bits.segment
  vrf.instWriteReport.bits.eew := laneRequest.bits.loadStoreEEW
  vrf.instWriteReport.bits.ls := laneRequest.bits.loadStore
  vrf.instWriteReport.bits.st := laneRequest.bits.store
  vrf.instWriteReport.bits.narrow := laneRequest.bits.decodeResult(Decoder.narrow)
  vrf.instWriteReport.bits.widen := laneRequest.bits.decodeResult(Decoder.widen)
  vrf.instWriteReport.bits.stFinish := false.B
  vrf.csrInterface := csrInterface
  vrf.lsuLastReport := lsuLastReport
  vrf.bufferClear := lsuVRFWriteBufferClear
  instructionFinished := instructionFinishedVec.reduce(_ | _)
}
