package v

import chisel3._
import chisel3.util._
import tilelink.{TLBundle, TLBundleParameter, TLChannelAParameter, TLChannelDParameter}

case class MSHRParam(ELEN: Int = 32, VLEN: Int = 1024, lane: Int = 8, vaWidth: Int = 32) {
  val dataBits:      Int = log2Ceil(ELEN)
  val mshrSize:      Int = 3
  val VLMaxBits:     Int = log2Ceil(VLEN) + 1
  val laneGroupSize: Int = VLEN / lane
  // 一次完全的offset读会最多分成多少个offset
  val lsuGroupLength:   Int = ELEN * lane / 8
  val offsetCountSize:  Int = log2Ceil(lsuGroupLength)
  val lsuGroupSize:     Int = VLEN / ELEN
  val lsuGroupSizeBits: Int = log2Ceil(lsuGroupSize) + 1
  val sourceWidth = 8

  val maskGroupWidth:    Int = lsuGroupLength
  val maskGroupSize:     Int = VLEN / maskGroupWidth
  val maskGroupSizeBits: Int = log2Ceil(maskGroupSize)
  val tlParam: TLBundleParameter = TLBundleParameter(
    a = TLChannelAParameter(vaWidth, sourceWidth, ELEN, 2, 4),
    b = None,
    c = None,
    d = TLChannelDParameter(sourceWidth, sourceWidth, ELEN, 2),
    e = None
  )
  def vrfParam: VRFParam = VRFParam(VLEN, lane, ELEN)
}

class MSHRStatus(lane: Int) extends Bundle {
  val instIndex:     UInt = UInt(3.W)
  val idle:          Bool = Bool()
  val indexGroupEnd: Bool = Bool()
  val targetLane:    UInt = UInt(lane.W)
  val waitFirstResp: Bool = Bool()
  val last:          Bool = Bool()
}

class MSHR(param: MSHRParam) extends Module {
  val req:              ValidIO[LSUReq] = IO(Flipped(Valid(new LSUReq(param.ELEN))))
  val readDataPort:     DecoupledIO[VRFReadRequest] = IO(Decoupled(new VRFReadRequest(param.vrfParam)))
  val readResult:       UInt = IO(Input(UInt(param.ELEN.W)))
  val offsetReadResult: Vec[ValidIO[UInt]] = IO(Vec(param.lane, Flipped(Valid(UInt(param.ELEN.W)))))
  val maskRegInput:     UInt = IO(Input(UInt(param.maskGroupWidth.W)))
  val maskSelect:       ValidIO[UInt] = IO(Valid(UInt(param.maskGroupSizeBits.W)))
  val tlPort:           TLBundle = IO(param.tlParam.bundle())
  val vrfWritePort:     DecoupledIO[VRFWriteRequest] = IO(Decoupled(new VRFWriteRequest(param.vrfParam)))
  val csrInterface:     LaneCsrInterface = IO(Input(new LaneCsrInterface(param.VLMaxBits)))
  val status:           MSHRStatus = IO(Output(new MSHRStatus(param.lane)))

  //  val addressBaseVec: UInt = RegInit(0.U(param.dataWidth.W))
  val indexOffsetVec: Vec[ValidIO[UInt]] = RegInit(
    VecInit(Seq.fill(param.lane)(0.U.asTypeOf(Valid(UInt(param.ELEN.W)))))
  )

  // 进请求
  val requestReg:     LSUReq = RegEnable(req.bits, 0.U.asTypeOf(req.bits), req.valid)
  val groupIndex:     UInt = RegInit(0.U(param.lsuGroupSizeBits.W))
  val nextGroupIndex: UInt = Mux(req.valid, 0.U, groupIndex + 1.U) //todo: vstart
  val firstReq:       Bool = RegEnable(req.valid, false.B, req.valid || tlPort.a.fire)
  val waitFirstResp:  Bool = RegEnable(req.valid && req.bits.instInf.fof, false.B, req.valid || tlPort.d.fire)

  // 处理offset的寄存器
  val offsetGroupIndexNext: UInt = Wire(UInt(2.W))
  val offsetGroupIndex:     UInt = RegEnable(offsetGroupIndexNext, req.valid || offsetReadResult.head.valid)
  offsetGroupIndexNext := Mux(req.valid, 3.U(2.W), offsetGroupIndex + 1.U)
  val offsetUsed: Vec[Bool] = Wire(Vec(param.lane, Bool()))
  indexOffsetVec.zipWithIndex.foreach {
    case (offset, index) =>
      offset.valid := offsetReadResult(index).valid || (offset.valid && !offsetUsed(index))
      offset.bits := Mux(offsetReadResult(index).valid, offsetReadResult(index).bits, offset.bits)
  }

  // data 存储, 暂时不 bypass 给 tile link
  val dataReg: UInt = RegEnable(readResult, 0.U, readDataPort.fire)

  // 缓存 mask, todo
  val maskReg: UInt = RegEnable(maskRegInput, 0.U, maskSelect.fire || req.valid)

  // 标志哪些做完了
  val reqDone: UInt = RegInit(0.U(param.lsuGroupLength.W))
  val segMask: UInt = RegInit(0.U(8.W))
  // 还没有d通道回应的请求
  val respDone:   UInt = RegInit(0.U(param.lsuGroupLength.W))
  val respFinish: Bool = respDone === 0.U

  val idle :: sRequest :: wResp :: Nil = Enum(3)
  val state: UInt = RegInit(idle)

  val segType: Bool = requestReg.instInf.nf.orR
  // unit stride 里面有额外的 mask 类型的补充
  val extendMaskType: Bool = requestReg.instInf.mop === 0.U && requestReg.instInf.vs2(0)
  // 所有的mask类型
  val maskType:  Bool = extendMaskType || requestReg.instInf.mask
  val indexType: Bool = requestReg.instInf.mop(0)
  val segNext:   UInt = ((segMask ## true.B) & (~segMask).asUInt)(7, 0)
  val segEnd:    Bool = OHToUInt(segNext) === requestReg.instInf.nf

  // 更新 segMask
  when((segType && tlPort.a.fire) || req.valid) {
    segMask := Mux(segEnd || req.valid, 0.U, segMask | segNext)
  }
  // 更新 reqDone
  val reqNext:      UInt = Wire(UInt(param.lsuGroupLength.W))
  val reqNextIndex: UInt = OHToUInt(reqNext)
  val segExhausted: Bool = tlPort.a.fire && (!segType || segEnd)
  val newGroup:     Bool = WireDefault(false.B)
  when(segExhausted || newGroup) {
    // todo: 应该由 vstart 决定初始值, 但是现在暂时不考虑异常
    reqDone := Mux(newGroup, 0.U, scanRightOr(reqNext))
  }

  // 额外添加的mask类型对数据的粒度有限制
  val dataEEW:       UInt = Mux(indexType, csrInterface.vSew, Mux(extendMaskType, 0.U, requestReg.instInf.eew))
  val dataEEWOH:     UInt = UIntToOH(dataEEW)
  val reqSize:       UInt = dataEEW
  val segmentIndex:  UInt = OHToUInt(segNext)
  val segmentOffset: UInt = (segmentIndex << dataEEW).asUInt

  /** offset index + segment index
    * log(32)-> 5    log(8) -> 3
    */
  val reqSource:     UInt = Mux(segType, reqNextIndex ## segmentIndex, reqNextIndex)
  val reqSource1H:   UInt = UIntToOH(reqSource(4, 0))
  val reqOffset:     UInt = Wire(UInt(param.ELEN.W))
  val reqValid:      Bool = Wire(Bool())
  val putData:       UInt = Wire(UInt(param.ELEN.W))
  val segAddressMul: UInt = (requestReg.instInf.nf + 1.U) * (1.U << dataEEW).asUInt(2, 0)
  val reqAddress:    UInt = requestReg.rs1Data + reqOffset * segAddressMul + segmentOffset
  val reqMask:       UInt = Wire(UInt(param.dataBits.W))

  tlPort.a.bits.opcode := !requestReg.instInf.st ## 0.U(2.W)
  tlPort.a.bits.param := 0.U
  tlPort.a.bits.size := reqSize
  tlPort.a.bits.source := reqSource
  tlPort.a.bits.address := reqAddress
  tlPort.a.bits.mask := reqMask
  tlPort.a.bits.data := Mux(requestReg.instInf.st, putData, 0.U)
  tlPort.a.bits.corrupt := false.B
  tlPort.a.valid := reqValid

  // 选出 reqNext
  val reqFilter:   UInt = (~reqDone).asUInt
  val maskFilter:  UInt = Wire(UInt(param.maskGroupWidth.W))
  val maskNext:    UInt = ffo(maskFilter)
  val reqDoneNext: UInt = (reqDone ## true.B) & reqFilter
  maskFilter := maskReg & reqFilter
  reqNext := Mux(maskType, maskNext, reqDoneNext)

  val maskReq: Bool = maskFilter === 0.U && maskType
  maskSelect.valid := maskReq

  /** 处理 offset
    * 每次每一条lane读会读出一组32bit的offset,汇集在这边就会有32 * lane的长度
    * 由与index在eew != 0的时候与mask(reqNext)不是一组对一组, [[indexOffset]] 记录了对应关系
    * 高位代表相mask被分的组数,低位代表组内偏移:
    *   eew = 0 的时候,index的element与mask是一一对应的
    *   eew = 1 的时候,一组mask对应两组index,此时[[indexOffset(reqNextIndex.getWidth)]]需要与[[offsetGroupIndex(0)]]匹配
    *   eew = 2 时,一组mask对应四组index，[[indexOffset]]高两位需要与[[offsetGroupIndex]]匹配
    * [[indexOffsetByteIndex]]表示这一次使用的index位于[[indexOffsetVec]]中的起始byte
    * [[indexOffsetNext]] 是通过 [[indexOffsetVec]] 移位得到我们想要的 index
    */
  val offsetEEW:              UInt = requestReg.instInf.eew
  val indexOffset:            UInt = (reqNextIndex << offsetEEW).asUInt(reqNextIndex.getWidth + 1, 0)
  val indexOffsetTargetGroup: UInt = indexOffset(reqNextIndex.getWidth + 1, reqNextIndex.getWidth)
  val indexOffsetByteIndex:   UInt = indexOffset(reqNextIndex.getWidth - 1, 0)
  val indexOffsetNext: UInt =
    (VecInit(indexOffsetVec.map(_.bits)).asUInt >> (indexOffsetByteIndex ## 0.U(3.W))).asUInt(param.ELEN - 1, 0)
  val offsetValidCheck: Bool =
    (VecInit(indexOffsetVec.map(_.valid)).asUInt >> (indexOffsetByteIndex >> 2).asUInt).asUInt(0)
  val groupMatch:       UInt = indexOffsetTargetGroup ^ offsetGroupIndex
  val offsetGroupCheck: Bool = (!offsetEEW(0) || !groupMatch(0)) && (!offsetEEW(1) || groupMatch === 0.U)
  val unitOffsetNext:   UInt = groupIndex ## reqNextIndex
  val strideOffsetNext: UInt = (groupIndex ## reqNextIndex) * requestReg.rs2Data
  reqOffset := Mux(
    indexType,
    indexOffsetNext,
    Mux(requestReg.instInf.mop(1), strideOffsetNext, unitOffsetNext)
  )

  // 拉回 indexOffset valid
  val indexLaneMask: UInt = UIntToOH(indexOffsetByteIndex(4, 2))
  val indexExhausted: Bool =
    Mux1H(UIntToOH(offsetEEW)(2, 0), Seq(indexOffsetByteIndex(1, 0).andR, indexOffsetByteIndex(1), true.B))

  /**
    * 各个类型的换组的标志:
    * 1. 如果是seg的类型,那么需要执行完完整的一组才算是结算的时间点:[[segEnd]]
    * 2. 如果是mask类型的,在被mask筛选之后没有剩余请求了,将直接换组: [[maskReq]].
    * 3. 如果是index类型的,那么在index耗尽的时候需要更换index,只有在index的粒度8的时候index和mask才同时耗尽.
    * 4. 如果index与mask不匹配,index在mask中的偏移由[[offsetGroupIndex]]记录.
    * 5. unit stride 和 stride 类型的在没有mask的前提下 [[reqNext]] 最高位拉高才换组.
    */
  val nextIndexReq: Bool = indexType && indexLaneMask(7) && indexExhausted
  // index 类型的需要在最后一份index被耗尽的时候通知lane读下一组index过来.
  status.indexGroupEnd := nextIndexReq && tlPort.a.fire && indexOffsetVec.last.valid
  indexOffsetVec.zipWithIndex.foreach {
    case (d, i) =>
      offsetUsed(i) := segExhausted && indexLaneMask(i) && indexExhausted
  }
  val groupEnd: Bool = (maskReq || reqNext(param.maskGroupWidth - 1)) && tlPort.a.fire

  // 处理 tile link source id 的冲突
  val sourceFree: Bool = !(reqSource1H & respDone).orR

  // stall 判断
  val stateCheck: Bool = state === sRequest
  // 如果状态是wResp,为了让回应能寻址会暂时不更新groupIndex，但是属于groupIndex的请求已经发完了
  val elementID: UInt = Mux(stateCheck, groupIndex, nextGroupIndex) ## reqNextIndex
  // todo: evl: unit stride -> whole register load | mask load, EEW=8
  val last:       Bool = (elementID >= csrInterface.vl) && segNext(0)
  val maskCheck:  Bool = !maskType || !maskReq
  val indexCheck: Bool = !indexType || (offsetValidCheck && offsetGroupCheck)
  val fofCheck:   Bool = firstReq || !waitFirstResp
  val dataReady:  Bool = !requestReg.instInf.st || readDataPort.ready
  val stateReady: Bool = stateCheck && maskCheck && indexCheck && fofCheck && sourceFree && !last
  reqValid := stateReady && dataReady

  // 处理回应
  // todo: 这里能工作，但是时间点不那么准确
  val respSourceOH: UInt = UIntToOH(tlPort.d.bits.source(4, 0))
  val lastResp:     Bool = last && tlPort.d.fire && (respDone & (~respSourceOH).asUInt) === 0.U
  val lastReq:      Bool = last && RegNext(tlPort.a.fire)
  val accessElementIndex: UInt = Mux(
    requestReg.instInf.st,
    reqNextIndex,
    Mux(
      segType,
      (tlPort.d.bits.source >> 3).asUInt,
      tlPort.d.bits.source
    )(param.offsetCountSize - 1, 0)
  )

  /** 首先我们根据source来计算 [[elementIndex]]
    * 然后我们左移 eew,得到修改的起始是第几个byte: [[baseByteOffset]]
    * XXX XXXXXXXX XX
    *     XXX: vd的增量
    *     |      |: 在一个寄存器中位于哪一个32bit
    *              XX：起始位置在32bit中的偏移
    *          XXX： 由于寄存器的实体按32bit为粒度分散在lane中,所以这是目标lane的index
    *        XX： 这里代表[[vrfWritePort.bits.offset]]
    */
  val elementIndex:   UInt = groupIndex ## accessElementIndex
  val baseByteOffset: UInt = (elementIndex << dataEEW).asUInt(9, 0)
  vrfWritePort.valid := tlPort.d.valid
  tlPort.d.ready := vrfWritePort.ready
  vrfWritePort.bits.vd := requestReg.instInf.vs3 +
    Mux(segType, Mux(requestReg.instInf.st, segmentIndex, tlPort.d.bits.source(2, 0)), baseByteOffset(9, 7))
  vrfWritePort.bits.offset := baseByteOffset(6, 5)
  vrfWritePort.bits.data := (tlPort.d.bits.data << (baseByteOffset(1, 0) ## 0.U(3.W))).asUInt
  vrfWritePort.bits.last := last
  vrfWritePort.bits.instructionIndex := requestReg.instIndex
  vrfWritePort.bits.mask := Mux1H(
    dataEEWOH(2, 0),
    Seq(
      UIntToOH(baseByteOffset(1, 0)),
      baseByteOffset(1) ## baseByteOffset(1) ## !baseByteOffset(1) ## !baseByteOffset(1),
      15.U(4.W)
    )
  )
  reqMask := vrfWritePort.bits.mask

  val sourceUpdate:     UInt = Mux(tlPort.a.fire, reqSource1H, 0.U(param.lsuGroupLength.W))
  val respSourceUpdate: UInt = Mux(tlPort.d.fire, respSourceOH, 0.U(param.lsuGroupLength.W))
  // 更新 respDone
  when((tlPort.d.fire || tlPort.a.fire) && !requestReg.instInf.st) {
    respDone := (respDone | sourceUpdate) & (~respSourceUpdate).asUInt
  }

  // state 更新
  when(state === sRequest) {
    when(last) {
      when(respFinish) {
        state := idle
      }.otherwise {
        state := wResp
      }
    }.elsewhen(groupEnd) {
      when(respFinish) {
        groupIndex := nextGroupIndex
        newGroup := true.B
      }.otherwise {
        state := wResp
      }
    }
  }

  when(state === wResp && respFinish) {
    when(last) {
      state := idle
    }.otherwise {
      state := sRequest
      groupIndex := nextGroupIndex
      newGroup := true.B
    }
  }
  when(req.valid) {
    state := sRequest
    groupIndex := nextGroupIndex
    newGroup := true.B
  }
  status.instIndex := requestReg.instIndex
  status.idle := state === idle
  status.last := Mux(requestReg.instInf.st, lastReq, lastResp)
  status.targetLane := UIntToOH(baseByteOffset(4, 2))
  status.waitFirstResp := waitFirstResp
  maskSelect.bits := groupIndex
  putData := readResult
  readDataPort.valid := stateReady && requestReg.instInf.st
  readDataPort.bits.offset := vrfWritePort.bits.offset
  readDataPort.bits.vs := vrfWritePort.bits.vd
  readDataPort.bits.instIndex := requestReg.instIndex
  // todo: maskSelect, last, targetLane
}
