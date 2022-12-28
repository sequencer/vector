package tests.elaborate

import chisel3._
import v.V
class CoverModule(dut: V) extends Module with TapModule {
  dut.laneVec.zipWithIndex.foreach { case (lane, index) =>
    val vrfWriteValid = cover(tap(lane.vrf.write.valid), s"lane${index} VRF write.")
  }
  done()
}