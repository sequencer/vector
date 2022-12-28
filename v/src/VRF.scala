package v

import chisel3._
import chisel3.util._

case class VRFParam(
  VLEN:           Int,
  lane:           Int,
  ELEN:           Int,
  vrfReadPort:    Int = 6,
  chainingSize:   Int = 4,
  writeQueueSize: Int = 4) {
  val regNum:     Int = 32
  val regNumBits: Int = log2Ceil(regNum)
  // One more bit for sorting
  val instIndexSize: Int = log2Ceil(chainingSize) + 1
  // 需要可配一行的宽度
  val portFactor: Int = 1
  val rowWidth:   Int = ELEN * portFactor
  val rfDepth:    Int = VLEN * regNum / rowWidth / lane
  val rfAddBits:  Int = log2Ceil(rfDepth)
  // 单个寄存器能分成多少组, 每次只访问32bit
  val singleGroupSize: Int = VLEN / ELEN / lane
  // 记录一个寄存器的offset需要多长
  val offsetBits: Int = log2Ceil(singleGroupSize)
  val rfBankNum:  Int = rowWidth / 8
  val maxVSew:    Int = log2Ceil(ELEN / 8)
  val VLMaxWidth: Int = log2Ceil(VLEN) + 1

  def rfParam: RFParam = RFParam(rfDepth)
}

class VRFReadRequest(param: VRFParam) extends Bundle {
  // 为了方便处理seg类型的ld st, vs需要是明确的地址, 而不是一个base
  val vs: UInt = UInt(param.regNumBits.W)
  // 访问寄存器的 offset, 代表第几个32bit
  val offset: UInt = UInt(param.offsetBits.W)
  // 用来阻塞 raw
  val instIndex: UInt = UInt(param.instIndexSize.W)
}

class VRFWriteRequest(param: VRFParam) extends Bundle {
  val vd:     UInt = UInt(param.regNumBits.W)
  val offset: UInt = UInt(param.offsetBits.W)
  // mask/ld类型的有可能不会写完整的32bit
  val mask:      UInt = UInt(4.W)
  val data:      UInt = UInt(param.ELEN.W)
  val last:      Bool = Bool()
  val instructionIndex: UInt = UInt(param.instIndexSize.W)
}

class VRFWriteReport(param: VRFParam) extends Bundle {
  val vd:        ValidIO[UInt] = Valid(UInt(param.regNumBits.W))
  val vs1:       ValidIO[UInt] = Valid(UInt(param.regNumBits.W))
  val vs2:       UInt = UInt(param.regNumBits.W)
  val instIndex: UInt = UInt(param.instIndexSize.W)
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

class VRF(param: VRFParam) extends Module {
  val read:            Vec[DecoupledIO[VRFReadRequest]] = IO(Vec(param.vrfReadPort, Flipped(Decoupled(new VRFReadRequest(param)))))
  val readResult:      Vec[UInt] = IO(Output(Vec(param.vrfReadPort, UInt(param.ELEN.W))))
  val write:           DecoupledIO[VRFWriteRequest] = IO(Flipped(Decoupled(new VRFWriteRequest(param))))
  val instWriteReport: DecoupledIO[VRFWriteReport] = IO(Flipped(Decoupled(new VRFWriteReport(param))))
  val flush:           Bool = IO(Input(Bool()))
  val csrInterface:    LaneCsrInterface = IO(Input(new LaneCsrInterface(param.VLMaxWidth)))
  val lsuLastReport:   ValidIO[UInt] = IO(Flipped(Valid(UInt(param.instIndexSize.W))))
  // write queue empty
  val bufferClear: Bool = IO(Input(Bool()))
  // todo: delete
  dontTouch(write)
  write.ready := true.B

  val chainingRecord: Vec[ValidIO[VRFWriteReport]] = RegInit(
    VecInit(Seq.fill(param.chainingSize)(0.U.asTypeOf(Valid(new VRFWriteReport(param)))))
  )

  val vsOffsetMask: UInt = {
    // 不用管浮点的
    val mul = csrInterface.vlmul(1, 0)
    mul.andR ## mul(1) ## mul.orR
  }
  val vsBaseMask: UInt = 3.U(2.W) ## (~vsOffsetMask).asUInt
  def rawCheck(before: VRFWriteReport, after: VRFWriteReport): Bool = {
    before.vd.valid &&
    ((before.vd.bits === after.vs1.bits && after.vs1.valid) ||
    (before.vd.bits === after.vs2) ||
    (before.vd.bits === after.vd.bits && after.ma))
  }

  def regOffsetCheck(beforeVsOffset: UInt, beforeOffset: UInt, afterVsOffset: UInt, afterOffset: UInt): Bool = {
    (beforeVsOffset > afterVsOffset) || ((beforeVsOffset === afterVsOffset) && (beforeOffset > afterOffset))
  }

  /** @param read : 发起读请求的相应信息
    * @param readRecord : 发起读请求的指令的记录\
    * @param record : 要做比对的指令的记录
    * todo: 维护冲突表,免得每次都要算一次
    */
  def chainingCheck(read: VRFReadRequest, readRecord: VRFWriteReport, record: ValidIO[VRFWriteReport]): Bool = {
    // 先看新老
    val older = instIndexL(read.instIndex, record.bits.instIndex)
    val sameInst = read.instIndex === record.bits.instIndex

    // todo: 处理双倍的
    val vs:       UInt = read.vs & vsBaseMask
    val vsOffset: UInt = read.vs & vsOffsetMask
    val vd = readRecord.vd.bits

    val raw: Bool = record.bits.vd.valid && (vs === record.bits.vd.bits) &&
      !regOffsetCheck(record.bits.vdOffset, record.bits.offset, vsOffset, read.offset)
    val waw: Bool = readRecord.vd.valid && record.bits.vd.valid && readRecord.vd.bits === record.bits.vd.bits &&
      !regOffsetCheck(record.bits.vdOffset, record.bits.offset, vsOffset, read.offset)
    val offsetCheckFail: Bool = !regOffsetCheck(record.bits.vdOffset, record.bits.offset, vsOffset, read.offset)
    val war: Bool = readRecord.vd.valid &&
      (((vd === record.bits.vs1.bits) && record.bits.vs1.valid) || (vd === record.bits.vs2) ||
        ((vd === record.bits.vd.bits) && record.bits.ma)) && offsetCheckFail
    !((!older && (waw || raw || war)) && !sameInst && record.valid)
  }

  def enqCheck(enq: VRFWriteReport, record: ValidIO[VRFWriteReport]): Bool = {
    val recordBits = record.bits
    val raw: Bool = rawCheck(record.bits, enq)
    val war: Bool = rawCheck(enq, record.bits)
    val waw: Bool = enq.vd.valid && recordBits.vd.valid && enq.vd.valid && enq.vd.bits === recordBits.vd.bits

    /** 两种暂时处理不了的冲突
      * 自己会乱序写 & wax: enq.unOrderWrite && (war || waw)
      * 老的会乱序写 & raw: record.bits.unOrderWrite && raw
      * todo: ld 需要更大粒度的channing更新或检测,然后除开segment的ld就能chaining起来了
      */
    (!((enq.unOrderWrite && (war || waw)) || (record.bits.unOrderWrite && raw))) || !record.valid
  }

  // todo: 根据 portFactor 变形
  // first read
  val bankReadF:   Vec[Bool] = Wire(Vec(param.vrfReadPort, Bool()))
  val bankReadS:   Vec[Bool] = Wire(Vec(param.vrfReadPort, Bool()))
  val readResultF: Vec[UInt] = Wire(Vec(param.rfBankNum, UInt(8.W)))
  val readResultS: Vec[UInt] = Wire(Vec(param.rfBankNum, UInt(8.W)))
  // portFactor = 1 的可以直接握手
  read.zipWithIndex.foldLeft((false.B, false.B)) {
    case ((o, t), (v, i)) =>
      // 先找到自的record
      val readRecord = Mux1H(chainingRecord.map(_.bits.instIndex === v.bits.instIndex), chainingRecord.map(_.bits))
      val checkResult:  Bool = chainingRecord.map(r => chainingCheck(v.bits, readRecord, r)).reduce(_ && _)
      val validCorrect: Bool = v.valid && checkResult
      // TODO: 加信号名
      v.ready := !t && checkResult
      bankReadF(i) := validCorrect & !o
      bankReadS(i) := validCorrect & !t & o
      readResult(i) := Mux(o, readResultS.asUInt, readResultF.asUInt)
      (o || validCorrect, (validCorrect && o) || t)
  }

  val rfVec: Seq[RegFile] = Seq.tabulate(param.rfBankNum) { bank =>
    // rf instant
    val rf = Module(new RegFile(param.rfParam))
    // connect readPorts
    rf.readPorts.head.addr := Mux1H(bankReadF, read.map(r => r.bits.vs ## r.bits.offset))
    rf.readPorts.last.addr := Mux1H(bankReadS, read.map(r => r.bits.vs ## r.bits.offset))
    readResultF(bank) := rf.readPorts.head.data
    readResultS(bank) := rf.readPorts.last.data
    // connect writePort
    rf.writePort.valid := write.valid & write.bits.mask(bank)
    rf.writePort.bits.addr := write.bits.vd ## write.bits.offset
    rf.writePort.bits.data := write.bits.data(8 * bank + 7, 8 * bank)
    rf
  }

  val initRecord: ValidIO[VRFWriteReport] = WireDefault(0.U.asTypeOf(Valid(new VRFWriteReport(param))))
  initRecord.valid := true.B
  initRecord.bits := instWriteReport.bits
  val freeRecord: UInt = VecInit(chainingRecord.map(!_.valid)).asUInt
  val recordFFO:  UInt = ffo(freeRecord)
  val recordEnq:  UInt = Wire(UInt(param.chainingSize.W))
  instWriteReport.ready := chainingRecord.map(r => enqCheck(instWriteReport.bits, r)).reduce(_ && _)
  recordEnq := Mux(instWriteReport.fire, recordFFO, 0.U(param.chainingSize.W))

  chainingRecord.zipWithIndex.foreach {
    case (record, i) =>
      when(recordEnq(i)) {
        record := initRecord
      }
      when(write.valid && write.bits.instructionIndex === record.bits.instIndex && (write.bits.last || write.bits.mask(3))) {
        record.bits.offset := write.bits.offset
        record.bits.vdOffset := vsOffsetMask & write.bits.vd
        when(write.bits.last) {
          record.valid := false.B
        }
      }
      when(flush) {
        record.valid := false.B
      }
      when(lsuLastReport.valid && lsuLastReport.bits === record.bits.instIndex) {
        record.bits.stFinish := true.B
      }
      when(record.bits.stFinish && bufferClear && record.valid) {
        record.valid := false.B
      }
  }
}
