package core.backend.rename

import chisel3._
import core.Parameter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import simulation.Simulator._

class FreeListTest extends AnyFreeSpec with Matchers {
  "FreeListTest" in {
    implicit val p = Parameter()
    val res = simulate(new FreeList(p.NRPhyRegs)) { dut =>

    }
  }
}
