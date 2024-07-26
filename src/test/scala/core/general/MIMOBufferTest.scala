package core.general

import chisel3._
import chisel3.util.PopCount
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import simulation.Simulator._


class MIMOBufferTest extends AnyFreeSpec with Matchers {
  "MIMOBufferTest" in {
    val inWidth = 8
    val outWidth = 5
    val entries = 32
    val dataWidth = 64
    simulate(new MIMOBuffer(UInt(dataWidth.W), inWidth, outWidth, entries)) { dut =>
      var data = 0
      var pushNum = 0
      var popNum = 0
      for (pushWidth <- 1 to inWidth) {
        pushPopTest(pushWidth)
      }

      def pushPopTest(pushWidth: Int): Unit = {
        println(s"testing pushWidth $pushWidth")
        val refEntries = (entries - (inWidth - pushWidth)) / pushWidth * pushWidth
        // push pop test
        reset()
        dut.io.out.ready.poke(true.B)
        for (cycle <- 0 until 1000) {
          if (dut.io.in.valid.peekValue().asBigInt == 0) {
            for (i <- 0 until pushWidth) {
              push(i, data.U(dataWidth.W))
              data = data + 1
            }
          }
          stepUpdate()
        }
        // push capacity test
        reset()
        for (cycle <- 0 until 1000) {
          if (dut.io.in.valid.peekValue().asBigInt == 0) {
            for (i <- 0 until pushWidth) {
              push(i, data.U(dataWidth.W))
              data = data + 1
            }
          }
          stepUpdate()
        }
        assert(pushNum == refEntries)
        // pop capacity test
        dut.io.in.valid.poke(false.B)
        dut.io.out.ready.poke(true.B)
        for (cycle <- 0 until 1000) {
          stepUpdate()
        }
        assert(popNum == refEntries)
      }


      def reset(): Unit = {
        data = 0
        pushNum = 0
        popNum = 0
        dut.io.in.valid.poke(false.B)
        dut.io.in.bits.foreach(_.valid.poke(false.B))
        dut.io.out.ready.poke(false.B)
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.reset.poke(false.B)
      }


      def push(idx: Int, data: UInt) = {
        dut.io.in.valid.poke(true.B)
        dut.io.in.bits(idx).valid.poke(true.B)
        dut.io.in.bits(idx).bits.poke(data)
      }

      def stepUpdate() = {
        val inFire = (dut.io.in.ready.peekValue().asBigInt != 0) && (dut.io.in.valid.peekValue().asBigInt != 0)
        val outFire = (dut.io.out.ready.peekValue().asBigInt != 0) && (dut.io.out.valid.peekValue().asBigInt != 0)
        if (outFire) {
          dut.io.out.bits.foreach(v => if (v.valid.peekValue().asBigInt != 0) popNum = popNum + 1)
        }
        dut.clock.step()
        if (inFire) {
          dut.io.in.bits.foreach(v => if (v.valid.peekValue().asBigInt != 0) pushNum = pushNum + 1)
          dut.io.in.valid.poke(false.B)
          dut.io.in.bits.foreach(_.valid.poke(false.B))
        }
      }


    }
  }
}