package core.backend.rename


import chisel3._
import chisel3.simulator.EphemeralSimulator._
import core.Parameter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class RenameTest extends AnyFreeSpec with Matchers {
  "RenameTest" in {
    implicit val p = Parameter()
    simulate(new RenameTable()) { dut =>
      val ref = Array.tabulate(p.NRArchRegs)(i => i)

      dut.io.write.foreach(w => w.valid.poke(false.B))
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()
      readAll()
      writeTable(0, 1, 33)
      writeTable(1, 1, 34)
      writeTable(2, 1, 35)
      writeTable(3, 2, 36)
      writeTable(4, 1, 37)
      readTable(0, 0, 1)
      readTable(0, 1, 2)
      checkStep()
      clearWrite()
      readAll()


      def writeTable(portIdx: Int, addr: Int, data: Int): Unit = {
        dut.io.write(portIdx).valid.poke(true.B)
        dut.io.write(portIdx).addr.poke(addr)
        dut.io.write(portIdx).data.poke(data)
        ref(addr) = data
      }

      def clearWrite(): Unit = {
        //invalid write port
        dut.io.write.foreach(w => w.valid.poke(false.B))
      }

      def readTable(portIdx: Int, idx: Int, addr: Int): Unit = {
        dut.io.read(portIdx)(idx).addr.poke(addr)
      }

      def readAll(): Unit = {
        clearWrite()
        for (i <- 0 until (p.NRArchRegs)) {
          dut.io.read.flatten.foreach(r => r.addr.poke(i))
          checkStep()
        }
      }
      def checkStep(): Unit = {
        dut.clock.step()
        checkRead()
      }
      def checkRead(): Unit = {
        dut.io.read.flatten.foreach(r => {
          val addr = r.addr.peekValue().asBigInt.toInt
          r.data.expect(ref(addr), s"rat[${addr}] error")
        })
      }
    }
  }


  def readBypassTest(dut: RenameTable, ref: Array[Int])(implicit p: Parameter): Unit = {
    dut.io.write.foreach(w => w.valid.poke(false.B))
  }


}
