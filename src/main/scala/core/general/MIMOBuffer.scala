package core.general

import chisel3.{Mux, _}
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
    val result = Wire(cloneType)
    if (isPow2(entries)) {
      result := (Cat(flag, value) + v).asTypeOf(this)
    } else {
      val valueNext = Cat(0.U(1.W), value) + v
      val reverseFlag = valueNext >= entries.U
      result.flag := flag ^ reverseFlag
      result.value := Mux(reverseFlag, valueNext - entries.U, valueNext)
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

class MIMOBuffer[T <: Data](gen: T, inWidth: Int, outWidth: Int, entries: Int, inCompact: Boolean = false, enableCheck: Boolean = true) extends Module {
  require(isPow2(entries))
  val io = IO(new MIMOBufferIO[T](gen, inWidth, outWidth))

  val bankBits = log2Up(inWidth.max(outWidth))
  val bankNum = 1 << bankBits
  val addrWidth = log2Up(entries)
  val srams = Array.tabulate(bankNum)(i => SyncReadMem(entries / bankNum, gen))
  withReset(io.flush || reset.asBool) {
    val anyFire = io.in.fire || io.out.fire

    val ptrZero = 0.U.asTypeOf(CirclePtr(entries))
    val rptrNext = Wire(CirclePtr(entries))
    val wptrNext = Wire(CirclePtr(entries))
    val rptr = RegEnable(rptrNext, ptrZero, io.out.fire)
    val wptr = RegEnable(wptrNext, ptrZero, io.in.fire)

    val outCount = PopCount(io.out.bits.map(_.valid))
    val inCount = PopCount(io.in.bits.map(_.valid))

    rptrNext := rptr + outCount
    wptrNext := wptr + inCount

    val rptrMem = Mux(io.out.fire, rptrNext, rptr)
    val wptrMem = Mux(io.in.fire, wptrNext, wptr)
    val readEntryNumNext = wptr - rptrMem
    val writeEntryNumNext = wptrMem - rptrMem
    //sram read bank
    val readBaseAddr = rptrMem.asUInt
    val readOffset = readBaseAddr(bankBits - 1, 0)
    val readAddr = VecInit.tabulate(bankNum)(bank => readBaseAddr(addrWidth - 1, bankBits) + (bank.U < readOffset))
    val readIndex = VecInit.tabulate(bankNum)(bank => Mux(bank.U < readOffset, bankNum.U + bank.U - readOffset, bank.U - readOffset))
    val readEnable = VecInit.tabulate(bankNum)(bank => readIndex(bank) < readEntryNumNext)
    val readData = VecInit.tabulate(bankNum)(bank => srams(bank).read(readAddr(bank), readEnable(bank)))
    //sram write bank
    val writeReq = VecInit.tabulate(bankNum)(bank => if (bank < inWidth) if (inCompact) io.in.bits(PopCount(io.in.bits.map(_.valid).take(bank))) else io.in.bits(bank) else 0.U.asTypeOf(ValidIO(gen)))
    val writeBaseAddr = wptr.asUInt
    val writeOffset = writeBaseAddr(bankBits - 1, 0)
    val writeAddr = VecInit.tabulate(bankNum)(bank => writeBaseAddr(addrWidth - 1, bankBits) + (bank.U < writeOffset))
    val writeIndex = VecInit.tabulate(bankNum)(bank => Mux(bank.U < writeOffset, bankNum.U + bank.U - writeOffset, bank.U - writeOffset)(bankBits - 1, 0))
    val writeEnable = VecInit.tabulate(bankNum)(bank => (writeIndex(bank) < inCount) && writeReq(writeIndex(bank)).valid && io.in.fire)
    val writeData = VecInit.tabulate(bankNum)(bank => writeReq(writeIndex(bank)).bits)
    writeEnable.zipWithIndex.foreach { case (wen, i) =>
      when(wen) {
        srams(i).write(writeAddr(i), writeData(i))
      }
    }

    //generate out data
    val outIndex = VecInit.tabulate(outWidth)(i => {
      val idx = readOffset + i.U
      RegEnable(Mux(idx < bankNum.U, idx, idx - bankNum.U), anyFire)
    })
    val readyNext = writeEntryNumNext <= (entries - inWidth).U
    val validNext = readEntryNumNext > 0.U
    io.in.ready := RegNext(readyNext, false.B)
    io.out.valid := RegEnable(validNext, false.B, anyFire)
    io.out.bits.zipWithIndex.foreach { case (v, i) =>
      v.bits := readData(outIndex(i))
      v.valid := RegEnable(i.U < readEntryNumNext, false.B, anyFire)
    }
  }
  if (enableCheck) {
    Module(new MIMOBufferCheck(gen, inWidth, outWidth, entries)).io := io
  }
}

class MIMOBufferCheck[T <: Data](gen: T, inWidth: Int, outWidth: Int, entries: Int) extends Module {
  val io = IO(Input(new MIMOBufferIO[T](gen, inWidth, outWidth)))
  val mem = Mem(entries, gen)
  val addrWidth = log2Up(entries)

  val refOut = VecInit.tabulate(outWidth)(_ => WireInit(0.U.asTypeOf(gen)))
  dontTouch(refOut)
  withReset(io.flush || reset.asBool) {
    val rptr = RegInit(0.U(addrWidth.W))
    val wptr = RegInit(0.U(addrWidth.W))

    when(io.out.fire) {
      rptr := rptr + PopCount(io.out.bits.map(_.valid))
      io.out.bits.zipWithIndex.foreach { case (v, i) =>
        when(v.valid) {
          refOut(i) := mem.read(rptr + i.U)
          assert(v.bits === refOut(i), s"out port $i error")
        }
      }
    }
    when(io.in.fire) {
      wptr := wptr + PopCount(io.in.bits.map(_.valid))
      io.in.bits.zipWithIndex.foreach { case (v, i) =>
        when(v.valid) {
          mem.write(wptr + i.U, v.bits)
        }
      }
    }
  }
}

object MIMOBuffer extends App {
  ChiselStage.emitSystemVerilogFile(new MIMOBuffer(UInt(128.W), 3, 7, 128))
}


