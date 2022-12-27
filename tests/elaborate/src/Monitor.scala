package tests.elaborate

import v.V
class Monitor(dut: V) extends TapModule {
  dut.laneVec.zipWithIndex.foreach { case (lane, index) =>
    val vrfWriteTap = tap(lane.vrf.write.bits).suggestName(s"lane${index}_vrfWriteTap")
  }
  done()
}