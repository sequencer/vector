package v

import chisel3._
import chisel3.util._
import tilelink.TLBundle

class MSHRStatus extends Bundle {
  val instIndex: UInt = UInt(3.W)
  val idle:      Bool = Bool()
  val groupEnd:  Bool = Bool()
}

class MSHR(param: LSUParam) extends Module {
  val req:              ValidIO[LSUReq] = IO(Flipped(Valid(new LSUReq(param))))
  val readDataPort:     DecoupledIO[VRFReadRequest] = IO(Decoupled(new VRFReadRequest(param.vrfParam)))
  val readResult:       UInt = IO(Input(UInt(param.ELEN.W)))
  val offsetReadResult: Vec[ValidIO[UInt]] = IO(Vec(param.lane, Flipped(Valid(UInt(param.ELEN.W)))))
  val maskRegInput:     UInt = IO(Input(UInt(param.maskGroupWidth.W)))
  val maskSelect:       DecoupledIO[UInt] = IO(Decoupled(UInt(param.maskGroupSizeBits.W)))
  val tlPort:           TLBundle = IO(param.tlParam.bundle())
  val vrfWritePort:     ValidIO[VRFWriteRequest] = IO(Valid(new VRFWriteRequest(param.vrfParam)))
  val csrInterface:     LaneCsrInterface = IO(Input(new LaneCsrInterface(param.VLMaxBits)))
  val status:           MSHRStatus = IO(Output(new MSHRStatus))

  //  val addressBaseVec: UInt = RegInit(0.U(param.dataWidth.W))
  val indexOffset: Vec[ValidIO[UInt]] = RegInit(
    VecInit(Seq.fill(param.lane)(0.U.asTypeOf(Valid(UInt(param.ELEN.W)))))
  )

  // 进请求
  val requestReg:    LSUReq = RegEnable(req.bits, 0.U.asTypeOf(req.bits), req.valid)
  val groupIndex:    UInt = RegInit(0.U(param.lsuGroupSizeBits.W))
  val firstReq:      Bool = RegEnable(req.valid, false.B, req.valid || tlPort.a.fire)
  val waitFirstResp: Bool = RegEnable(req.valid && req.bits.instInf.fof, false.B, req.valid || tlPort.d.fire)

  // 处理offset的寄存器
  val offsetUsed: Vec[Bool] = Wire(Vec(param.lane, Bool()))
  indexOffset.zipWithIndex.foreach {
    case (offset, index) =>
      offset.valid := offsetReadResult(index).valid || (offset.valid && !offsetUsed(index))
      offset.bits := Mux(offsetReadResult(index).valid, offsetReadResult(index).bits, offset.bits)
  }

  // data 存储, 暂时不 bypass 给 tile link
  val dataReg: UInt = RegEnable(readResult, 0.U, readDataPort.fire)

  // 缓存 mask
  val maskReg: UInt = RegEnable(maskRegInput, 0.U, maskSelect.fire || req.valid)

  // 标志哪些做完了
  val reqDone:    UInt = RegInit(0.U(param.lsuGroupLength.W))
  val segMask:    UInt = RegInit(0.U(8.W))
  val respDone:   UInt = RegInit(0.U(param.lsuGroupSize.W))
  val respFinish: Bool = respDone === 0.U

  val idle :: sRequest :: wResp :: Nil = Enum(3)
  val state: UInt = RegInit(idle)
  val initStateWire = WireInit(VecInit(Seq(!requestReg.instInf.st, false.B)))

  val segType:   Bool = requestReg.instInf.nf.orR
  val maskType:  Bool = requestReg.instInf.mop === 0.U && requestReg.instInf.vs2(0)
  val indexType: Bool = requestReg.instInf.mop(0)
  val segNext:   UInt = (segMask ## true.B) & (~segMask).asUInt
  val segEnd:    Bool = OHToUInt(segNext) === requestReg.instInf.nf

  // 更新 segMask
  when((segType && tlPort.a.fire) || req.valid) {
    segMask := Mux(segEnd || req.valid, 0.U, segMask | segNext)
  }
  // 更新 reqDone
  val reqNext:      UInt = Wire(UInt(param.lsuGroupLength.W))
  val reqNextIndex: UInt = OHToUInt(reqNext)
  val segExhausted: Bool = tlPort.a.fire && (!segType || segEnd)
  when(segExhausted || req.valid) {
    // todo: 应该由 vstart 决定初始值, 但是现在暂时不考虑异常
    reqDone := Mux(req.valid, 0.U, scanRightOr(reqNext))
  }

  // 连a通道
  val dataEEW:       UInt = Mux(indexType, csrInterface.vSew, requestReg.instInf.eew)
  val dataEEWOH:     UInt = UIntToOH(dataEEW)
  val reqSize:       UInt = dataEEW
  val segmentIndex:  UInt = OHToUInt(segNext)
  val segmentOffset: UInt = (segmentIndex << dataEEW).asUInt

  /** offset index + segment index
    * log(32)-> 5    log(8) -> 3
    */
  val reqSource:   UInt = Mux(segType, reqNextIndex ## segmentIndex, reqNextIndex)
  val reqSource1H: UInt = UIntToOH(reqSource(4, 0))
  val reqOffset:   UInt = Wire(UInt(param.ELEN.W))
  val reqValid:    Bool = Wire(Bool())
  val putData:     UInt = Wire(UInt(param.ELEN.W))
  // AGU: segmentOffset 只有 6 bit, 这里需要特别处理
  val reqAddress: UInt = requestReg.rs1Data + reqOffset + segmentOffset
  val reqMask:    UInt = dataEEWOH(2) ## dataEEWOH(2) ## (dataEEWOH(2) || dataEEWOH(1)) ## true.B

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
  val maskFilter:  UInt = maskReg & reqFilter
  val maskReq:     Bool = maskFilter === 0.U && maskType
  val maskNext:    UInt = ffo(maskFilter)
  val reqDoneNext: UInt = (reqDone ## true.B) & reqFilter
  reqNext := Mux(maskType, maskNext, reqDoneNext)
  maskSelect.valid := maskReq

  // 选出 offset
  /**
    * mask 类型的 eew 恒定是 8
    */
  val offsetEEW: UInt = Mux(maskType, 0.U, requestReg.instInf.eew)
  val bytIndex:  UInt = (reqNextIndex << offsetEEW).asUInt
  val indexOffsetNext: UInt =
    (VecInit(indexOffset.map(_.bits)).asUInt >> (bytIndex ## 0.U(3.W))).asUInt(param.ELEN - 1, 0)
  val offsetValidCheck: Bool = (VecInit(indexOffset.map(_.valid)).asUInt >> (bytIndex >> 2).asUInt).asUInt(0)
  val unitOffsetNext:   UInt = groupIndex ## bytIndex
  val strideOffsetNext: UInt = (groupIndex ## reqNextIndex) * requestReg.rs2Data
  reqOffset := Mux(
    indexType,
    indexOffsetNext,
    Mux(requestReg.instInf.mop(1), strideOffsetNext, unitOffsetNext)
  )

  // 拉回 indexOffset valid
  val indexLaneMask:  UInt = UIntToOH(bytIndex(4, 2))
  val indexExhausted: Bool = Mux1H(UIntToOH(offsetEEW)(2, 0), Seq(bytIndex(1, 0).andR, bytIndex(1), true.B))
  status.groupEnd := indexLaneMask(7) && indexExhausted && tlPort.a.fire && indexOffset.last.valid
  indexOffset.zipWithIndex.foreach {
    case (d, i) =>
      when(segExhausted && indexLaneMask(i) && indexExhausted) {
        assert(!req.valid)
        d.valid := false.B
      }
  }

  // 处理 tile link source id 的冲突
  val sourceFree: Bool = !(reqSource1H & respDone).orR

  // stall 判断
  reqValid := (state === sRequest) && (!maskType || !maskReq) && (!indexType || offsetValidCheck) &&
    sourceFree && (firstReq || !waitFirstResp)

  // 处理回应
  val last:       Bool = WireDefault(false.B)
  val respSinkOH: UInt = UIntToOH(tlPort.d.bits.sink(4, 0))
  vrfWritePort.valid := tlPort.d.valid
  tlPort.d.ready := true.B
  vrfWritePort.bits.vd := requestReg.instInf.vs3 + Mux(segType, tlPort.d.bits.sink(2, 0), 0.U)
  vrfWritePort.bits.groupIndex := Mux(
    segType,
    groupIndex ## (tlPort.d.bits.sink >> 3).asUInt,
    groupIndex ## tlPort.d.bits.sink
  )
  vrfWritePort.bits.eew := dataEEW
  vrfWritePort.bits.data := tlPort.d.bits.data
  vrfWritePort.bits.last := last

  // 更新 respDone
  when(tlPort.d.fire && tlPort.a.fire && !requestReg.instInf.st) {
    respDone := (respDone | reqSource1H) & (~respSinkOH).asUInt
  }

  // state 更新
  val nextGroupIndex: UInt = Mux(req.valid, 0.U, groupIndex + 1.U) //todo: vstart
  when(state === sRequest && status.groupEnd) {
    when(respFinish) {
      when(last) {
        state := idle
      }.otherwise {
        groupIndex := nextGroupIndex
      }
    }.otherwise {
      state := wResp
    }
  }

  when(state === wResp && respFinish) {
    when(last) {
      state := idle
    }.otherwise {
      state := sRequest
      groupIndex := nextGroupIndex
    }
  }
  when(req.valid) {
    state := sRequest
    groupIndex := nextGroupIndex
  }
  status.instIndex := requestReg.instIndex
  status.idle := state === idle
  // todo: add state wMask && read store data
  maskSelect := DontCare
}