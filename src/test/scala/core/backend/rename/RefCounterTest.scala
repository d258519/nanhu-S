package core.backend.rename

import chisel3._
import core.Parameter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import simulation.MSimulator._


class RefCounterTest extends AnyFreeSpec with Matchers {
  "RefCounterTest" in {
    implicit val p = Parameter()
    val res = simulate(new RefCounter) { dut =>
      val refCounter = Array.tabulate(p.NRPhyRegs)(i => if (i < p.NRArchRegs) 1 else 0)
      val dutCounter = Array.fill(p.NRPhyRegs)(0)

      reset()
      check()
      allocate(0, 0)
      allocate(1, 0)
      allocate(2, 0)
      stepCheck()
      allocate(1, 0)
      stepCheck()
      allocate(0, 0)
      stepCheck()
      allocate(2, 1)
      stepCheck()
      allocate(2, 2)
      allocate(3, 2)
      stepCheck()
      free(0, 0)
      free(1, 0)
      free(2, 0)
      free(3, 0)
      free(4, 0)
      free(5, 0)
      free(6, 1)
      free(7, 1)
      stepCheck()
      free(1, 2)
      stepCheck()
      free(2, 2)
      free(5, 2)
      stepCheck()
      // free all reg
      reset()
      for (i <- 0 until p.NRArchRegs) {
        val port = i % p.CommitWidth
        free(port, i)
        if (port == 7) {
          stepCheck()
        }
      }
      stepCheck()
      // multi free check
      for (i <- 0 until p.RenameWidth) {
        allocate(i, 128)
      }
      stepCheck()
      for (i <- 0 until p.RenameWidth) {
        free(i, 128)
      }
      stepCheck()

      def free(port: Int, data: Int): Unit = {
        assert(port < p.CommitWidth)
        dut.io.in.valid.poke(true.B)
        dut.io.in.bits(port).valid.poke(true.B)
        dut.io.in.bits(port).bits.poke(data.U)
      }

      def stepCheck(): Unit = {
        dut.clock.step()
        check()
        clear()
      }

      def allocate(port: Int, data: Int): Unit = {
        assert(port < p.RenameWidth)
        dut.io.allocate(port).valid.poke(true.B)
        dut.io.allocate(port).bits.poke(data.U)
      }

      def clear(): Unit = {
        dut.io.in.valid.poke(false.B)
        dut.io.in.bits.foreach(_.valid.poke(false.B))
        dut.io.allocate.foreach(_.valid.poke(false.B))
      }

      def reset(): Unit = {
        clear()
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
        refCounter.zipWithIndex.foreach { case (v, i) => refCounter(i) = if (i < p.NRArchRegs) 1 else 0 }
      }

      def check(): Unit = {
        dut.io.allocate.foreach(v =>
          if (v.valid.peekValue().asBigInt != 0) {
            val idx = v.bits.peekValue().asBigInt.toInt
            refCounter(idx) = refCounter(idx) + 1
          }
        )
        if (dut.io.in.valid.peekValue().asBigInt != 0) {

          dut.io.in.bits.zipWithIndex.foreach { case (v, i) =>
            if (v.valid.peekValue().asBigInt != 0) {
              val idx = v.bits.peekValue().asBigInt.toInt
              refCounter(idx) = refCounter(idx) - 1
            }
          }
          val freeValid = Array.fill(p.CommitWidth)(false)
          val freeReg = Array.fill(p.CommitWidth)(0)
          dut.io.in.bits.zipWithIndex.foreach { case (v, i) =>
            if (v.valid.peekValue().asBigInt != 0) {
              val idx = v.bits.peekValue().asBigInt.toInt
              freeValid(i) = refCounter(idx) == 0
              freeReg(i) = v.bits.peekValue().asBigInt.toInt
            }
          }
          for (i <- 0 until p.CommitWidth) {
            if (freeValid(i)) {
              for (j <- 0 until i) {
                if (freeValid(j) && freeReg(i) == freeReg(j)) {
                  freeValid(i) = false
                }
              }
            }
          }
          val outValid = freeValid.reduce(_ | _) && (dut.io.in.valid.peekValue().asBigInt != 0)
          dut.io.out.valid.expect(outValid.B)
          dut.io.out.bits.zipWithIndex.foreach { case (v, i) =>
            v.valid.expect(freeValid(i).B, s"out port $i valid error")
            if (freeValid(i)) v.bits.expect(freeReg(i).U, s"out port $i bits error")
          }
        }
      }


    }
  }
}
