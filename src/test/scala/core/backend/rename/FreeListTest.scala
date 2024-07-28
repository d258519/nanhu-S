package core.backend.rename

import chisel3._
import core.Parameter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import simulation.Simulator._

import scala.util.Random

class FreeListTest extends AnyFreeSpec with Matchers {
  "FreeListTest" in {
    implicit val p = Parameter()
    val res = simulate(new FreeList(p.NRPhyRegs)) { dut =>
      var r = 0
      reset()
      dut.io.allocate.req.valid.poke(true.B)
      dut.io.allocate.req.bits.foreach(_.poke(true.B))
      dut.clock.step(100)
      clear()
      pushPopTest()
      clear()
      pushPopTest(1)
      clear()
      pushPopTest(2)
      clear()
      pushPopTest(1, 1)
      clear()
      pushPopTest(2, 2)
      clear()
      pushPopTest(3, 2)
      clear()
      dut.io.allocate.req.valid.poke(true.B)
      dut.io.allocate.req.bits.foreach(_.poke(true.B))
      dut.clock.step(100)
      clear()
      dut.io.allocate.req.valid.poke(true.B)
      dut.io.allocate.req.bits.foreach(_.poke(true.B))
      dut.io.free.valid.poke(true.B)
      dut.io.free.bits.foreach(_.valid.poke(true.B))

      for (i <- 0 to 999) {
        dut.io.free.bits.foreach(_.valid.poke(false.B))
        for (port <- 0 to Random.nextInt(p.CommitWidth)) {
          dut.io.free.bits(port).valid.poke(true.B)
          dut.io.free.bits(port).bits.poke(r.U)
          r = (r + 1) % p.NRPhyRegs
        }
        dut.clock.step()
      }

      def pushPopTest(allocatePorts: Int = p.RenameWidth, freePorts: Int = p.CommitWidth): Unit = {
        var phyReg = 0
        while (phyReg < p.NRPhyRegs - p.CommitWidth) {
          for (i <- 0 until freePorts) {
            if (phyReg < p.NRPhyRegs - p.CommitWidth) {
              free(i, phyReg.U)
              phyReg = phyReg + 1
            }

          }
          dut.clock.step()
          clear()
        }
        for (i <- 0 until allocatePorts) {
          allocate(i)
        }
        dut.clock.step(100)
      }


      def free(port: Int, data: UInt): Unit = {
        dut.io.free.valid.poke(true.B)
        dut.io.free.bits(port).bits.poke(data)
        dut.io.free.bits(port).valid.poke(true.B)
      }

      def allocate(port: Int): Unit = {
        dut.io.allocate.req.valid.poke(true.B)
        dut.io.allocate.req.bits(port).poke(true.B)
      }

      def reset(): Unit = {
        clear()
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
      }

      def clear(): Unit = {
        dut.io.allocate.req.valid.poke(false.B)
        dut.io.allocate.req.bits.foreach(_.poke(false.B))
        dut.io.free.valid.poke(false.B)
        dut.io.free.bits.foreach(_.valid.poke(false.B))
      }

    }
  }
}
