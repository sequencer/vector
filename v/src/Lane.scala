package v

import chisel3._
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.util._

object LaneParameter {
  implicit def rwP: upickle.default.ReadWriter[LaneParameter] = upickle.default.macroRW
}
case class LaneParameter(vLen: Int, datapathWidth: Int, laneNumber: Int, chainingSize: Int, vrfWriteQueueSize: Int)
    extends SerializableModuleParameter {
  val instructionIndexSize: Int = log2Ceil(chainingSize) + 1
  val lmulMax:              Int = 8
  val sewMin:               Int = 8
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

  /** compare to next group number. */
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

  def vrfParam:         VRFParam = VRFParam(vLen, laneNumber, datapathWidth)
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
    Flipped(Decoupled(new VRFReadRequest(parameter.vrfParam)))
  )
  val vrfReadDataChannel: UInt = IO(Output(UInt(parameter.datapathWidth.W)))
  val vrfWriteChannel:    DecoupledIO[VRFWriteRequest] = IO(Flipped(Decoupled(new VRFWriteRequest(parameter.vrfParam))))

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

  dontTouch(writeBusPort)
  val maskGroupedOrR: UInt = VecInit(maskInput.asBools.grouped(4).toSeq.map(VecInit(_).asUInt.orR)).asUInt
  val vrf:            VRF = Module(new VRF(parameter.vrfParam))
  // reg
  val controlValid: Vec[Bool] = RegInit(VecInit(Seq.fill(parameter.chainingSize)(false.B)))
  // read from vs1
  val source1: Vec[UInt] = RegInit(VecInit(Seq.fill(parameter.chainingSize)(0.U(parameter.datapathWidth.W))))
  // read from vs2
  val source2: Vec[UInt] = RegInit(VecInit(Seq.fill(parameter.chainingSize)(0.U(parameter.datapathWidth.W))))
  // read from vd
  val source3: Vec[UInt] = RegInit(VecInit(Seq.fill(parameter.chainingSize)(0.U(parameter.datapathWidth.W))))
  // execute result
  val result: Vec[UInt] = RegInit(VecInit(Seq.fill(parameter.chainingSize)(0.U(parameter.datapathWidth.W))))
  // 跨lane写的额外用寄存器存储执行的结果和mask
  val crossWriteResultHead: UInt = RegInit(0.U(parameter.datapathWidth.W))
  val crossWriteMaskHead:   UInt = RegInit(0.U(2.W))
  val crossWriteResultTail: UInt = RegInit(0.U(parameter.datapathWidth.W))
  val crossWriteMaskTail:   UInt = RegInit(0.U(2.W))
  // 额外给 lsu 和 mask unit
  val rfWriteVec: Vec[ValidIO[VRFWriteRequest]] = Wire(
    Vec(parameter.chainingSize + 1, Valid(new VRFWriteRequest(parameter.vrfParam)))
  )
  rfWriteVec(4).valid := vrfWriteChannel.valid
  rfWriteVec(4).bits := vrfWriteChannel.bits
  val rfWriteFire: UInt = Wire(UInt((parameter.chainingSize + 2).W))
  vrfWriteChannel.ready := rfWriteFire(4)
  val maskRequestVec: Vec[ValidIO[UInt]] = Wire(
    Vec(parameter.chainingSize, Valid(UInt(parameter.maskGroupSizeWidth.W)))
  )
  val maskRequestFire: UInt = Wire(UInt(parameter.chainingSize.W))
  // 跨lane操作的寄存器
  // 从rf里面读出来的， 下一个周期试图上环
  val crossReadHeadTX: UInt = RegInit(0.U(parameter.HLEN.W))
  val crossReadTailTX: UInt = RegInit(0.U(parameter.HLEN.W))
  // 从环过来的， 两个都好会拼成source2
  val crossReadHeadRX: UInt = RegInit(0.U(parameter.HLEN.W))
  val crossReadTailRX: UInt = RegInit(0.U(parameter.HLEN.W))
  val control: Vec[InstControlRecord] = RegInit(
    VecInit(Seq.fill(parameter.chainingSize)(0.U.asTypeOf(new InstControlRecord(parameter))))
  )

  // wire
  val vrfReadWire: Vec[Vec[DecoupledIO[VRFReadRequest]]] = Wire(
    Vec(parameter.chainingSize, Vec(3, Decoupled(new VRFReadRequest(parameter.vrfParam))))
  )
  val vrfReadResult:   Vec[Vec[UInt]] = Wire(Vec(parameter.chainingSize, Vec(3, UInt(parameter.datapathWidth.W))))
  val controlActive:   Vec[Bool] = Wire(Vec(parameter.chainingSize, Bool()))
  val controlCanShift: Vec[Bool] = Wire(Vec(parameter.chainingSize, Bool()))
  // 读的环index与这个lane匹配上了, 会出环
  val readBusDeq: ValidIO[ReadBusData] = Wire(Valid(new ReadBusData(parameter: LaneParameter)))

  // 以6个执行单元为视角的控制信号
  val executeEnqValid:  Vec[UInt] = Wire(Vec(parameter.chainingSize, UInt(parameter.executeUnitNum.W)))
  val executeEnqFire:   UInt = Wire(UInt(parameter.executeUnitNum.W))
  val executeDeqFire:   UInt = Wire(UInt(parameter.executeUnitNum.W))
  val executeDeqData:   Vec[UInt] = Wire(Vec(parameter.executeUnitNum, UInt(parameter.datapathWidth.W)))
  val instTypeVec:      Vec[UInt] = Wire(Vec(parameter.chainingSize, UInt(parameter.executeUnitNum.W)))
  val instWillComplete: Vec[Bool] = Wire(Vec(parameter.chainingSize, Bool()))
  val maskReqValid:     Vec[Bool] = Wire(Vec(parameter.chainingSize, Bool()))
  // 往执行单元的请求
  val logicRequests: Vec[LaneLogicRequest] = Wire(
    Vec(parameter.chainingSize, new LaneLogicRequest(parameter.datePathParam))
  )
  val adderRequests: Vec[LaneAdderReq] = Wire(Vec(parameter.chainingSize, new LaneAdderReq(parameter.datePathParam)))
  val shiftRequests: Vec[LaneShifterReq] = Wire(
    Vec(parameter.chainingSize, new LaneShifterReq(parameter.shifterParameter))
  )
  val mulRequests:   Vec[LaneMulReq] = Wire(Vec(parameter.chainingSize, new LaneMulReq(parameter.mulParam)))
  val divRequests:   Vec[LaneDivRequest] = Wire(Vec(parameter.chainingSize, new LaneDivRequest(parameter.datePathParam)))
  val otherRequests: Vec[OtherUnitReq] = Wire(Vec(parameter.chainingSize, Output(new OtherUnitReq(parameter))))
  val maskRequests:  Vec[LaneDataResponse] = Wire(Vec(parameter.chainingSize, Output(new LaneDataResponse(parameter))))
  val endNoticeVec:  Vec[UInt] = Wire(Vec(parameter.chainingSize, UInt(parameter.chainingSize.W)))

  // 作为最老的坑的控制信号
  val sendReady:      Bool = Wire(Bool())
  val sendWriteReady: Bool = Wire(Bool())
  val sendReadData:   ValidIO[ReadBusData] = Wire(Valid(new ReadBusData(parameter)))
  val sendWriteData:  ValidIO[WriteBusData] = Wire(Valid(new WriteBusData(parameter)))

  val vSewOrR: Bool = csrInterface.vSew.orR
  val sew1H:   UInt = UIntToOH(csrInterface.vSew)

  /** 符号的mask,外面好像不用处理符号 */
  val signMask = Seq(!vSewOrR, csrInterface.vSew(0))

  /** 不同 vSew 结束时候的index
    * 00 -> 11
    * 01 -> 10
    * 10 -> 00
    */
  val endIndex: UInt = !csrInterface.vSew(1) ## !vSewOrR

  // 跨lane写rf需要一个queue
  val crossWriteQueue: Queue[VRFWriteRequest] = Module(
    new Queue(new VRFWriteRequest(parameter.vrfParam), parameter.vrfWriteQueueSize)
  )

  control.zipWithIndex.foreach {
    case (record, index) =>
      // read only
      val decodeRes = record.originalInformation.decodeResult
      val decodeResFormat:    InstructionDecodeResult = decodeRes.asTypeOf(new InstructionDecodeResult)
      val decodeResFormatExt: ExtendInstructionDecodeResult = decodeRes.asTypeOf(new ExtendInstructionDecodeResult)
      val extendInst = decodeRes(19) && decodeRes(1, 0).orR
      val needCrossRead = !extendInst && decodeResFormat.firstWiden
      val needCrossWrite = !extendInst && decodeResFormat.Widen
      val dataDeq:     UInt = Mux1H(instTypeVec(index), executeDeqData)
      val dataDeqFire: Bool = (instTypeVec(index) & executeDeqFire).orR
      val firstMasked: Bool = Wire(Bool())
      when(needCrossRead) {
        assert(csrInterface.vSew != 2.U)
      }
      // 有mask或者不是mask类的指令
      val maskReady: Bool = record.mask.valid || !record.originalInformation.mask
      // 跨lane读写的指令我们只有到最老才开始做
      controlActive(index) := controlValid(
        index
      ) && controlValid.head && ((index == 0).B || !(needCrossRead || needCrossWrite)) && maskReady
      // todo: 能不能移动还需要纠结纠结
      controlCanShift(index) := !record.state.sExecute
      // vs1 read
      vrfReadWire(index)(0).valid := !record.state.sRead1 && controlActive(index)
      vrfReadWire(index)(0).bits.offset := record.counter(parameter.vrfOffsetWidth - 1, 0)
      // todo: 在 vlmul > 0 的时候需要做的是cat而不是+,因为寄存器是对齐的
      vrfReadWire(index)(0).bits.vs := record.originalInformation.vs1 + record.counter(
        parameter.groupNumberWidth - 1,
        parameter.vrfOffsetWidth
      )
      vrfReadWire(index)(0).bits.instIndex := record.originalInformation.instructionIndex
      // Mux(decodeResFormat.eew16, 1.U, csrInterface.vSew)

      // vs2 read
      vrfReadWire(index)(1).valid := !record.state.sRead2 && controlActive(index)
      vrfReadWire(index)(1).bits.offset := Mux(
        needCrossRead,
        record.counter(parameter.vrfOffsetWidth - 2, 0) ## false.B,
        record.counter(parameter.vrfOffsetWidth - 1, 0)
      )
      vrfReadWire(index)(1).bits.vs := record.originalInformation.vs2 + Mux(
        needCrossRead,
        record.counter(parameter.groupNumberWidth - 1, parameter.vrfOffsetWidth - 1) ## false.B,
        record.counter(parameter.groupNumberWidth - 1, parameter.vrfOffsetWidth)
      )
      vrfReadWire(index)(1).bits.instIndex := record.originalInformation.instructionIndex

      // vd read
      vrfReadWire(index)(2).valid := !record.state.sReadVD && controlActive(index)
      vrfReadWire(index)(2).bits.offset := Mux(
        needCrossRead,
        record.counter(parameter.vrfOffsetWidth - 2, 0) ## true.B,
        record.counter(parameter.vrfOffsetWidth - 1, 0)
      )
      vrfReadWire(index)(2).bits.vs := record.originalInformation.vd + Mux(
        needCrossRead,
        record.counter(parameter.groupNumberWidth - 1, parameter.vrfOffsetWidth - 1) ## true.B,
        record.counter(parameter.groupNumberWidth - 1, parameter.vrfOffsetWidth)
      )
      vrfReadWire(index)(2).bits.instIndex := record.originalInformation.instructionIndex

      val readFinish =
        record.state.sReadVD && record.state.sRead1 && record.state.sRead2 && record.state.wRead1 && record.state.wRead2

      // 处理读出来的结果
      when(vrfReadWire(index)(0).fire) {
        record.state.sRead1 := true.B
        // todo: Mux
        source1(index) := vrfReadResult(index)(0)
      }
      when(vrfReadWire(index)(1).fire) {
        record.state.sRead2 := true.B
        source2(index) := vrfReadResult(index)(1)
      }

      when(vrfReadWire(index)(2).fire) {
        record.state.sReadVD := true.B
        source3(index) := vrfReadResult(index)(2)
      }
      // 处理上环的数据
      if (index == 0) {
        val tryToSendHead = record.state.sRead2 && !record.state.sSendResult0 && controlValid.head
        val tryToSendTail = record.state.sReadVD && !record.state.sSendResult1 && controlValid.head
        sendReadData.bits.target := tryToSendTail ## laneIndex(parameter.laneNumberWidth - 1, 1)
        sendReadData.bits.tail := laneIndex(0)
        sendReadData.bits.instIndex := record.originalInformation.instructionIndex
        sendReadData.bits.data := Mux(tryToSendHead, crossReadHeadTX(0), crossReadTailTX(0))
        sendReadData.valid := tryToSendHead || tryToSendTail

        // 跨lane的写
        val sendWriteHead = record.state.sExecute && !record.state.sCrossWrite0 && controlValid.head
        val sendWriteTail = record.state.sExecute && !record.state.sCrossWrite1 && controlValid.head
        sendWriteData.bits.target := laneIndex(parameter.laneNumberWidth - 2, 0) ## (!sendWriteHead)
        sendWriteData.bits.from := laneIndex
        sendWriteData.bits.tail := laneIndex(parameter.laneNumberWidth - 1)
        sendWriteData.bits.instIndex := record.originalInformation.instructionIndex
        sendWriteData.bits.counter := record.counter
        sendWriteData.bits.data := Mux(sendWriteHead, crossWriteResultHead, crossWriteResultTail)
        sendWriteData.bits.mask := Mux(sendWriteHead, crossWriteMaskHead, crossWriteMaskTail)
        sendWriteData.valid := sendWriteHead || sendWriteTail

        // 跨lane读写的数据接收
        when(readBusDeq.valid) {
          assert(readBusDeq.bits.instIndex === record.originalInformation.instructionIndex)
          when(readBusDeq.bits.tail) {
            record.state.wRead2 := true.B
            crossReadTailRX := readBusDeq.bits.data
          }.otherwise {
            record.state.wRead1 := true.B
            crossReadHeadRX := readBusDeq.bits.data
          }
        }

        // 读环发送的状态变化
        // todo: 处理发给自己的, 可以在使用的时候直接用读的寄存器, init state的时候自己纠正回来
        when(sendReady) {
          record.state.sSendResult0 := true.B
          when(record.state.sSendResult0) {
            record.state.sSendResult1 := true.B
          }
        }
        // 写环发送的状态变化
        when(sendWriteReady && sendWriteData.valid) {
          record.state.sCrossWrite0 := true.B
          when(record.state.sCrossWrite0) {
            record.state.sCrossWrite1 := true.B
          }
        }

        // 跨lane的读记录
        when(vrfReadWire(index)(1).fire && decodeResFormat.firstWiden) {
          crossReadHeadTX := vrfReadResult(index)(1)
        }
        when(vrfReadWire(index)(2).fire && decodeResFormat.firstWiden) {
          crossReadTailTX := vrfReadResult(index)(2)
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
        when(dataDeqFire && !firstMasked) {
          when(record.executeIndex(1)) {
            // update tail
            crossWriteResultTail :=
              Mux(
                csrInterface.vSew(0),
                dataDeq(parameter.datapathWidth - 1, parameter.HLEN),
                Mux(
                  record.executeIndex(0),
                  dataDeq(parameter.HLEN - 1, 0),
                  crossWriteResultTail(parameter.datapathWidth - 1, parameter.HLEN)
                )
              ) ## Mux(
                !record.executeIndex(0) || csrInterface.vSew(0),
                dataDeq(parameter.HLEN - 1, 0),
                crossWriteResultTail(parameter.HLEN - 1, 0)
              )
            crossWriteMaskTail :=
              (record.executeIndex(0) || csrInterface.vSew(0) || crossWriteMaskTail(1)) ##
                (!record.executeIndex(0) || csrInterface.vSew(0) || crossWriteMaskTail(0))
          }.otherwise {
            // update head
            crossWriteResultHead :=
              Mux(
                csrInterface.vSew(0),
                dataDeq(parameter.datapathWidth - 1, parameter.HLEN),
                Mux(
                  record.executeIndex(0),
                  dataDeq(parameter.HLEN - 1, 0),
                  crossWriteResultHead(parameter.datapathWidth - 1, parameter.HLEN)
                )
              ) ## Mux(
                !record.executeIndex(0) || csrInterface.vSew(0),
                dataDeq(parameter.HLEN - 1, 0),
                crossWriteResultHead(parameter.HLEN - 1, 0)
              )
            crossWriteMaskHead :=
              (record.executeIndex(0) || csrInterface.vSew(0) || crossWriteMaskHead(1)) ##
                (!record.executeIndex(0) || csrInterface.vSew(0) || crossWriteMaskHead(0))
          }

        }
        when(record.state.asUInt.andR) {
          crossWriteMaskHead := 0.U
          crossWriteMaskTail := 0.U
        }
      }
      // 发起执行单元的请求
      /** 计算结果需要偏移的: executeIndex * 8 */
      val dataOffset: UInt = record.executeIndex ## 0.U(3.W)

      /** 正在算的是这个lane的第多少个 element */
      val elementIndex: UInt = Mux1H(
        sew1H(2, 0),
        Seq(
          (record.counter ## record.executeIndex)(4, 0),
          (record.counter ## record.executeIndex(1))(4, 0),
          record.counter
        )
      )

      /** 我们默认被更新的 [[record.counter]] & [[record.executeIndex]] 对应的 element 是没有被 mask 掉的
        * 但是这会有一个意外：在更新mask的时候会导致第一个被 mask 掉了但是会试图执行
        * 等到更新完选完 mask 组再去更新 [[record.counter]] & [[record.executeIndex]] 感觉不是科学的做法
        * 所以特别处理一下这种情况
        */
      firstMasked := record.originalInformation.mask && record.mask.valid && (elementIndex(
        4,
        0
      ) === 0.U) && !record.mask.bits(0)
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
        sew1H(1, 0),
        Seq(
          record.counter(parameter.groupNumberWidth - 1, parameter.groupNumberWidth - 3),
          false.B ## record.counter(parameter.groupNumberWidth - 1, parameter.groupNumberWidth - 2)
        )
      ) + maskNeedUpdate
      val indexInLane = nextGroupCountMSB ## nextIndex
      // csrInterface.vSew 只会取值0, 1, 2,需要特别处理移位
      val nextIntermediateVolume = (indexInLane << csrInterface.vSew).asUInt
      val nextGroupCount = nextIntermediateVolume(parameter.groupNumberWidth + 1, 2)
      val nextExecuteIndex = nextIntermediateVolume(1, 0)

      /** 虽然没有计算完一组,但是这一组剩余的都被mask去掉了 */
      val maskFilterEnd = record.originalInformation.mask && (nextGroupCount =/= record.counter)

      /** 需要一个除vl导致的end来决定下一个的 element index 是什么 */
      val dataDepletion = record.executeIndex === endIndex || maskFilterEnd

      /** 这一组计算全完成了 */
      val groupEnd = dataDepletion || instWillComplete(index)

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
        sew1H(2, 0),
        Seq(
          UIntToOH(record.executeIndex),
          record.executeIndex(1) ## record.executeIndex(1) ## !record.executeIndex(1) ## !record.executeIndex(1),
          15.U(4.W)
        )
      )
      val executeBitEnable: UInt = FillInterleaved(8, executeByteEnable)
      def CollapseOperand(data: UInt, enable: Bool = true.B, sign: Bool = false.B): UInt = {
        val dataMasked: UInt = data & executeBitEnable
        val select:     UInt = Mux(enable, sew1H(2, 0), 4.U(3.W))
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
      val finalSource1 = CollapseOperand(source1(index), decodeResFormat.vType, !decodeResFormat.unSigned0)

      /** source2 一定是V类型的 */
      val finalSource2 = CollapseOperand(source2(index), true.B, !decodeResFormat.unSigned1)

      /** source3 有两种：adc & ma, c等处理mask的时候再处理 */
      val finalSource3 = CollapseOperand(source3(index))
      // 假如这个单元执行的是logic的类型的,请求应该是什么样子的
      val logicRequest = Wire(new LaneLogicRequest(parameter.datePathParam))
      logicRequest.src.head := finalSource2
      logicRequest.src.last := finalSource1
      logicRequest.opcode := decodeResFormat.uop
      val nextElementIndex = Mux1H(
        sew1H,
        Seq(
          indexInLane(indexInLane.getWidth - 1, 2) ## laneIndex ## indexInLane(1, 0),
          indexInLane(indexInLane.getWidth - 1, 1) ## laneIndex ## indexInLane(0),
          indexInLane ## laneIndex
        )
      )
      instWillComplete(index) := nextElementIndex >= csrInterface.vl
      // 在手动做Mux1H
      logicRequests(index) := maskAnd(
        controlValid(index) && decodeResFormat.logicUnit && !decodeResFormat.otherUnit,
        logicRequest
      )

      // adder 的
      val adderRequest = Wire(new LaneAdderReq(parameter.datePathParam))
      adderRequest.src := VecInit(Seq(finalSource1, finalSource2, finalSource3))
      adderRequest.opcode := decodeResFormat.uop
      adderRequest.sign := !decodeResFormat.unSigned1
      adderRequest.reverse := decodeResFormat.reverse
      adderRequest.average := decodeResFormat.average
      adderRequests(index) := maskAnd(
        controlValid(index) && decodeResFormat.adderUnit && !decodeResFormat.otherUnit,
        adderRequest
      )

      // shift 的
      val shiftRequest = Wire(new LaneShifterReq(parameter.shifterParameter))
      shiftRequest.src := finalSource2
      shiftRequest.shifterSize := Mux1H(
        sew1H(2, 1),
        Seq(false.B ## finalSource1(3), finalSource1(4, 3))
      ) ## finalSource1(2, 0)
      shiftRequest.opcode := decodeResFormat.uop
      shiftRequests(index) := maskAnd(
        controlValid(index) && decodeResFormat.shiftUnit && !decodeResFormat.otherUnit,
        shiftRequest
      )

      // mul
      val mulRequest: LaneMulReq = Wire(new LaneMulReq(parameter.mulParam))
      mulRequest.src := VecInit(Seq(finalSource1, finalSource2, finalSource3))
      mulRequest.opcode := decodeResFormat.uop
      mulRequests(index) := maskAnd(
        controlValid(index) && decodeResFormat.mulUnit && !decodeResFormat.otherUnit,
        mulRequest
      )

      // div
      val divRequest = Wire(new LaneDivRequest(parameter.datePathParam))
      divRequest.src := VecInit(Seq(finalSource1, finalSource2))
      divRequest.rem := decodeResFormat.uop(0)
      divRequest.sign := decodeResFormat.unSigned0
      divRequests(index) := maskAnd(
        controlValid(index) && decodeResFormat.divUnit && !decodeResFormat.otherUnit,
        divRequest
      )

      // other
      val otherRequest: OtherUnitReq = Wire(Output(new OtherUnitReq(parameter)))
      otherRequest.src := VecInit(Seq(finalSource1, finalSource2))
      otherRequest.opcode := decodeResFormat.uop(2, 0)
      otherRequest.imm := record.originalInformation.vs1
      otherRequest.extendType.valid := decodeResFormat.uop(3)
      otherRequest.extendType.bits.elements.foreach { case (s, d) => d := decodeResFormatExt.elements(s) }
      otherRequest.laneIndex := laneIndex
      otherRequest.groupIndex := record.counter
      otherRequest.sign := !decodeResFormat.unSigned0
      otherRequests(index) := maskAnd(controlValid(index) && decodeResFormat.otherUnit, otherRequest)

      // 往scheduler的执行任务compress viota
      val maskRequest: LaneDataResponse = Wire(Output(new LaneDataResponse(parameter)))

      // viota & compress & ls 需要给外边数据
      val maskType: Bool =
        (record.originalInformation.special || record.originalInformation.loadStore) && controlActive(index)
      val maskValid = maskType && record.state.sRead2 && !record.state.sExecute
      // 往外边发的是原始的数据
      maskRequest.data := source2(index)
      maskRequest.toLSU := record.originalInformation.loadStore
      maskRequest.instructionIndex := record.originalInformation.instructionIndex
      maskRequests(index) := maskAnd(controlValid(index) && maskValid, maskRequest)
      maskReqValid(index) := maskValid

      when(
        laneResponseFeedback.valid && laneResponseFeedback.bits.instructionIndex === record.originalInformation.instructionIndex
      ) {
        record.state.wScheduler := true.B
      }
      instTypeVec(index) := record.originalInformation.instType
      executeEnqValid(index) := maskAnd(readFinish && !record.state.sExecute, instTypeVec(index))
      when((instTypeVec(index) & executeEnqFire).orR || maskValid) {
        when(groupEnd || maskValid) {
          record.state.sExecute := true.B
        }.otherwise {
          record.executeIndex := nextExecuteIndex
        }
      }

      // todo: 暂时先这样把,处理mask的时候需要修
      val executeResult = (dataDeq << dataOffset).asUInt(parameter.datapathWidth - 1, 0)
      val resultUpdate: UInt = (executeResult & executeBitEnable) | (result(index) & (~executeBitEnable).asUInt)
      when(dataDeqFire) {
        when(groupEnd) {
          record.state.wExecuteRes := true.B
        }
        result(index) := resultUpdate
        when(!firstMasked) {
          record.vrfWriteMask := record.vrfWriteMask | executeByteEnable
        }
      }
      // 写rf
      rfWriteVec(index).valid := record.state.wExecuteRes && !record.state.sWrite && controlActive(index)
      rfWriteVec(index).bits.vd := record.originalInformation.vd + record.counter(
        parameter.groupNumberWidth - 1,
        parameter.vrfOffsetWidth
      )
      rfWriteVec(index).bits.offset := record.counter
      rfWriteVec(index).bits.data := result(index)
      rfWriteVec(index).bits.last := instWillComplete(index)
      rfWriteVec(index).bits.instructionIndex := record.originalInformation.instructionIndex
      val notLastWrite = !instWillComplete(index)
      // 判断这一个lane是否在body与tail的分界线上,只有分界线上的才需要特别计算mask
      val dividingLine:    Bool = (csrInterface.vl << csrInterface.vSew).asUInt(4, 2) === laneIndex
      val useOriginalMask: Bool = notLastWrite || !dividingLine
      rfWriteVec(index).bits.mask := record.vrfWriteMask
      when(rfWriteFire(index)) {
        record.state.sWrite := true.B
      }
      endNoticeVec(index) := 0.U(parameter.chainingSize.W)
      val maskUnhindered = maskRequestFire(index) || !maskNeedUpdate
      when((record.state.asUInt.andR && maskUnhindered) || record.instCompleted) {
        when(instWillComplete(index) || record.instCompleted) {
          controlValid(index) := false.B
          when(controlValid(index)) {
            endNoticeVec(index) := UIntToOH(
              record.originalInformation.instructionIndex(parameter.instructionIndexSize - 2, 0)
            )
          }
        }.otherwise {
          record.state := record.initState
          record.counter := nextGroupCount
          record.executeIndex := nextExecuteIndex
          record.vrfWriteMask := 0.U
          when(maskRequestFire(index)) {
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
          controlValid(index) := false.B
        }
      }
      // mask 更换
      maskRequestVec(index).valid := maskNeedUpdate
      maskRequestVec(index).bits := nextGroupCountMSB
  }

  // 处理读环的
  {
    val readBusDataReg: ValidIO[ReadBusData] = RegInit(0.U.asTypeOf(Valid(new ReadBusData(parameter))))
    val readBusIndexMatch = readBusPort.enq.bits.target === laneIndex
    readBusDeq.valid := readBusIndexMatch && readBusPort.enq.valid
    readBusDeq.bits := readBusPort.enq.bits
    // 暂时优先级策略是环上的优先
    readBusPort.enq.ready := true.B
    readBusDataReg.valid := false.B

    when(readBusPort.enq.valid) {
      when(!readBusIndexMatch) {
        readBusDataReg.valid := true.B
        readBusDataReg.bits := readBusPort.enq.bits
      }
    }

    // 试图进环
    readBusPort.deq.valid := readBusDataReg.valid || sendReadData.valid
    readBusPort.deq.bits := Mux(readBusDataReg.valid, readBusDataReg.bits, sendReadData.bits)
    sendReady := !readBusDataReg.valid
  }

  // 处理写环
  {
    val writeBusDataReg: ValidIO[WriteBusData] = RegInit(0.U.asTypeOf(Valid(new WriteBusData(parameter))))
    // 策略依然是环上的优先,如果queue满了继续转
    val writeBusIndexMatch = writeBusPort.enq.bits.target === laneIndex && crossWriteQueue.io.enq.ready
    writeBusPort.enq.ready := true.B
    writeBusDataReg.valid := false.B
    crossWriteQueue.io.enq.bits.vd := control.head.originalInformation.vd + writeBusPort.enq.bits.counter(3, 1)
    crossWriteQueue.io.enq.bits.offset := writeBusPort.enq.bits.counter ## writeBusPort.enq.bits.tail
    crossWriteQueue.io.enq.bits.data := writeBusPort.enq.bits.data
    crossWriteQueue.io.enq.bits.last := instWillComplete.head && writeBusPort.enq.bits.tail
    crossWriteQueue.io.enq.bits.instructionIndex := control.head.originalInformation.instructionIndex
    crossWriteQueue.io.enq.bits.mask := FillInterleaved(2, writeBusPort.enq.bits.mask)
    //writeBusPort.enq.bits
    crossWriteQueue.io.enq.valid := false.B

    when(writeBusPort.enq.valid) {
      when(writeBusIndexMatch) {
        crossWriteQueue.io.enq.valid := true.B
      }.otherwise {
        writeBusDataReg.valid := true.B
        writeBusDataReg.bits := writeBusPort.enq.bits
      }
    }

    // 进写环
    writeBusPort.deq.valid := writeBusDataReg.valid || sendWriteData.valid
    writeBusPort.deq.bits := Mux(writeBusDataReg.valid, writeBusDataReg.bits, sendWriteData.bits)
    sendWriteReady := !writeBusDataReg.valid
  }

  // 执行单元
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
    mul.req := VecInit(mulRequests.map(_.asUInt)).reduce(_ | _).asTypeOf(new LaneMulReq(parameter.mulParam))
    div.req.bits := VecInit(divRequests.map(_.asUInt))
      .reduce(_ | _)
      .asTypeOf(new LaneDivRequest(parameter.datePathParam))
    otherUnit.req := VecInit(otherRequests.map(_.asUInt)).reduce(_ | _).asTypeOf(Output(new OtherUnitReq(parameter)))
    laneResponse.bits := VecInit(maskRequests.map(_.asUInt))
      .reduce(_ | _)
      .asTypeOf(Output(new LaneDataResponse(parameter)))
    laneResponse.valid := maskReqValid.asUInt.orR
    // 执行单元的其他连接
    adder.csr.vSew := csrInterface.vSew
    adder.csr.vxrm := csrInterface.vxrm
    otherUnit.csr.vSew := csrInterface.vSew
    otherUnit.csr.vxrm := csrInterface.vxrm
    div.mask := DontCare
    div.vSew := csrInterface.vSew

    // 连接执行结果
    executeDeqData := VecInit(
      Seq(logicUnit.resp, adder.resp, shifter.resp, mul.resp, div.resp.bits, otherUnit.resp.data)
    )
    executeDeqFire := executeEnqFire(5) ## div.resp.valid ## executeEnqFire(3, 0)
    // 执行单元入口握手
    val tryToUseExecuteUnit = VecInit(executeEnqValid.map(_.asBools).transpose.map(VecInit(_).asUInt.orR)).asUInt
    executeEnqFire := tryToUseExecuteUnit & (true.B ## div.req.ready ## 15.U(4.W))
    div.req.valid := tryToUseExecuteUnit(4)
  }

  // 处理 rf
  {
    // 连接读口
    val readArbiter = Module(new Arbiter(new VRFReadRequest(parameter.vrfParam), 8))
    // 暂时把lsu的读放在了最低优先级,有问题再改
    (vrfReadWire(1).last +: (vrfReadWire(2) ++ vrfReadWire(3)) :+ vrfReadAddressChannel)
      .zip(readArbiter.io.in)
      .foreach {
        case (source, sink) =>
          sink <> source
      }
    (vrfReadWire.head ++ vrfReadWire(1).init :+ readArbiter.io.out).zip(vrf.read).foreach {
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
    val normalWrite = VecInit(rfWriteVec.map(_.valid)).asUInt.orR
    val writeSelect = !normalWrite ## ffo(VecInit(rfWriteVec.map(_.valid)).asUInt)
    val writeEnqBits = Mux1H(writeSelect, rfWriteVec.map(_.bits) :+ crossWriteQueue.io.deq.bits)
    vrf.write.valid := normalWrite || crossWriteQueue.io.deq.valid
    vrf.write.bits := writeEnqBits
    crossWriteQueue.io.deq.ready := !normalWrite && vrf.write.ready
    rfWriteFire := Mux(vrf.write.ready, writeSelect, 0.U)

    //更新v0
    v0Update.valid := vrf.write.valid && writeEnqBits.vd === 0.U
    v0Update.bits.data := writeEnqBits.data
    v0Update.bits.offset := writeEnqBits.offset
    v0Update.bits.mask := writeEnqBits.mask
  }

  {
    // 处理mask的请求
    val maskSelectArbitrator = ffo(
      VecInit(maskRequestVec.map(_.valid)).asUInt ## (laneRequest.valid && laneRequest.bits.mask)
    )
    maskRequestFire := maskSelectArbitrator(parameter.chainingSize, 1)
    maskSelect := Mux1H(maskSelectArbitrator, 0.U.asTypeOf(maskRequestVec.head.bits) +: maskRequestVec.map(_.bits))
  }
  // 控制逻辑的移动
  val entranceControl: InstControlRecord = Wire(new InstControlRecord(parameter))
  val entranceFormat:  InstructionDecodeResult = laneRequest.bits.decodeResult.asTypeOf(new InstructionDecodeResult)
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
  entranceControl.counter := (csrInterface.vStart >> 3).asUInt
  // todo: spec 10.1: imm 默认是 sign-extend,但是有特殊情况
  val vs1entrance: UInt =
    Mux(
      entranceFormat.vType,
      0.U,
      Mux(
        entranceFormat.xType,
        laneRequest.bits.readFromScalar,
        VecInit(Seq.fill(parameter.datapathWidth - 5)(laneRequest.bits.vs1(4))).asUInt ## laneRequest.bits.vs1
      )
    )
  val entranceInstType: UInt = laneRequest.bits.instType
  // todo: 修改v0的和使用v0作为mask的指令需要产生冲突
  val typeReady: Bool = VecInit(
    instTypeVec.zip(controlValid).map { case (t, v) => (t =/= entranceInstType) || !v }
  ).asUInt.andR
  val validRegulate: Bool = laneRequest.valid && typeReady
  laneRequest.ready := !controlValid.head && typeReady && vrf.instWriteReport.ready
  vrf.instWriteReport.valid := (laneRequest.fire || (!laneRequest.bits.store && laneRequest.bits.loadStore)) && !entranceControl.instCompleted
  when(!controlValid.head && (controlValid.asUInt.orR || validRegulate)) {
    controlValid := VecInit(controlValid.tail :+ validRegulate)
    source1 := VecInit(source1.tail :+ vs1entrance)
    control := VecInit(control.tail :+ entranceControl)
    result := VecInit(result.tail :+ 0.U(parameter.datapathWidth.W))
    source2 := VecInit(source2.tail :+ 0.U(parameter.datapathWidth.W))
    source3 := VecInit(source3.tail :+ 0.U(parameter.datapathWidth.W))
    crossWriteMaskHead := 0.U
    crossWriteMaskTail := 0.U
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
  vrf.instWriteReport.bits.vs1.valid := entranceFormat.vType
  // TODO: move ma to [[V]]
  vrf.instWriteReport.bits.ma := laneRequest.bits.ma
  // 暂时认为ld都是无序写寄存器的
  vrf.instWriteReport.bits.unOrderWrite := (laneRequest.bits.loadStore && !laneRequest.bits.store) || entranceFormat.otherUnit
  vrf.instWriteReport.bits.seg.valid := laneRequest.bits.loadStore && laneRequest.bits.segment.orR
  vrf.instWriteReport.bits.seg.bits := laneRequest.bits.segment
  vrf.instWriteReport.bits.eew := laneRequest.bits.loadStoreEEW
  vrf.instWriteReport.bits.ls := laneRequest.bits.loadStore
  vrf.instWriteReport.bits.st := laneRequest.bits.store
  vrf.instWriteReport.bits.narrow := entranceFormat.narrow
  vrf.instWriteReport.bits.widen := entranceFormat.Widen
  vrf.instWriteReport.bits.stFinish := false.B
  vrf.csrInterface := csrInterface
  vrf.lsuLastReport := lsuLastReport
  vrf.bufferClear := lsuVRFWriteBufferClear
  instructionFinished := endNoticeVec.reduce(_ | _)
}
