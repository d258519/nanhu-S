import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class CounterTest extends AnyFreeSpec with Matchers {
  "CounterTest" in {
    simulate(new Counter()) { dut =>

      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)

      val TEST_CYCLE = 1000000
      val CNT_MAX = dut.CNT_MAX.litValue
      var cycles: Int = 0
      while (cycles < TEST_CYCLE) {
        val ref_led = (cycles / CNT_MAX) & 1
        dut.io.led.expect(ref_led, "led out failed at " + cycles)
        dut.clock.step()
        cycles += 1
      }
    }
  }
}