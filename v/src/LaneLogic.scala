package v

import chisel3._

class LaneLogicRequest(param: DataPathParam) extends Bundle {
  val src: Vec[UInt] = Vec(2, UInt(param.dataWidth.W))

  /** n_op ## op_n ## op */
  val opcode: UInt = UInt(4.W)
}

class LaneLogic(param: DataPathParam) extends Module {
  val req:  LaneLogicRequest = IO(Input(new LaneLogicRequest(param)))
  val resp: UInt = IO(Output(UInt(param.dataWidth.W)))

  resp := VecInit(req.src.map(_.asBools).transpose.map {
    case Seq(sr0, sr1) =>
      val bitCalculate = Module(new LaneBitLogic)
      bitCalculate.src := sr0 ## (req.opcode(2) ^ sr1)
      bitCalculate.opcode := req.opcode
      bitCalculate.resp ^ req.opcode(3)
  }).asUInt
}
