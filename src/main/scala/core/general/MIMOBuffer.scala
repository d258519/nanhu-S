package core.general

import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage

object CirclePtr {
  def apply(entries: Int): CirclePtr = new CirclePtr(entries)
}

class CirclePtr(val entries: Int) extends Bundle {
  val ptrWidth = log2Up(entries)
  val flag = Bool()
  val value = UInt(ptrWidth.W)


  def +(v: UInt): CirclePtr = {
    val result = Wire(this)
    if (isPow2(entries)) {
      result := Cat(flag, value) + v
    } else {
      val valueNext = Cat("b0".U, value) + v
      val reverseFlag = valueNext(ptrWidth)
      result.flag := flag ^ reverseFlag
      result.value := valueNext(ptrWidth - 1, 0)
    }
    result
  }

  def -(v: CirclePtr): UInt = {
    assert(entries == v.entries)
    Mux(flag === v.flag, value - v.value, entries.U + value - v.value)
  }
}

class MIMOBufferIO[T <: Data](gen: T, inWidth: Int, outWidth: Int) extends Bundle {
  val in = Flipped(DecoupledIO(Vec(inWidth, Flipped(ValidIO(gen)))))
  val out = DecoupledIO(Vec(outWidth, ValidIO(gen)))
  val flush = Input(Bool())
}

class MIMOBuffer[T <: Data](gen: T, inWidth: Int, outWidth: Int, entries: Int) extends Module {
  require(isPow2(entries))
  val io = IO(new MIMOBufferIO[T](gen, inWidth, outWidth))

  val bankNum = log2Up(inWidth).max(log2Up(outWidth))
  val mem = SyncReadMem(entries / bankNum, gen) //Vec(bankNum,SyncReadMem(entries/bankNum, gen))
  withReset(io.flush || reset.asBool) {
    val anyFire = io.in.fire || io.out.fire

    val ptrZero = WireDefault(0.U.asTypeOf(CirclePtr(entries)))
    val rptrNext, wptrNext = Wire(CirclePtr(entries))
    val rptr = RegEnable(rptrNext, ptrZero, io.out.fire)
    val wptr = RegEnable(wptrNext, ptrZero, io.in.fire)

    val outCount = PopCount(io.out.bits.map(_.valid))
    val inCount = PopCount(io.in.bits.map(_.valid))

    rptrNext := rptr + outCount.asUInt
    wptrNext := wptr + inCount.asUInt

    //    val rptrMem = Mux(io.out.fire, rptrNext, rptr)

    //    val entryNumNext = Wire(wptr - rptrMem)
    //    val entryNum = RegEnable(entryNumNext, ptrZero, anyFire)
    //    val readyNext = entryNumNext <= (entries - inWidth).U
    //    io.in.ready := RegEnable(readyNext, false.B, anyFire)


  }
}

object MIMOBuffer extends App {
  ChiselStage.emitSystemVerilogFile(new MIMOBuffer(UInt(128.W), 3, 7, 128))

}


