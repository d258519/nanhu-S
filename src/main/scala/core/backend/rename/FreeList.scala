package core.backend.rename

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import core._
import core.general._

class FreeListIO(implicit p: Parameter) extends CoreBundle {
  val allocate = new Bundle {
    val req = Flipped(DecoupledIO(Vec(RenameWidth, Bool())))
    val rsp = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
  }
  val free = Flipped(ValidIO(Vec(CommitWidth, ValidIO(UInt(PhyRegIdxWidth.W)))))
}

class FreeList(size: Int)(implicit p: Parameter) extends CoreModule {
  val io = IO(new FreeListIO)
  val freeList = RegInit(VecInit.tabulate(size)(i => (i + NRArchRegs).U(PhyRegIdxWidth.W)))
  val zeroPtr = 0.U.asTypeOf(CirclePtr(size))
  val rptrNext, wptrNext = WireInit(zeroPtr)
  val rptr = RegEnable(rptrNext, zeroPtr, io.allocate.req.fire)
  val wptr = RegEnable(wptrNext, (NRPhyRegs - NRArchRegs).U.asTypeOf(zeroPtr), io.free.valid)
  rptrNext := rptr + PopCount(io.allocate.req.bits)
  wptrNext := wptr + PopCount(io.free.bits.map(_.valid))
  val rptrOH = RegEnable(UIntToOH(rptrNext.value.asUInt, size), 1.U(size.W), io.allocate.req.fire)
  val rptrOHs = VecInit.tabulate(RenameWidth)(i => rptrOH.rotateLeft(PopCount(io.allocate.req.bits.take(i))))
  io.allocate.rsp.zipWithIndex.foreach { case (v, i) => v := Mux1H(rptrOHs(i), freeList) }
  val readyEntryNum = wptr - rptr
  // could move the 'ready' comparison logic to the front of the pointer register for better timing
  io.allocate.req.ready := readyEntryNum >= RenameWidth.U
  val freeListNextValidVec = VecInit.fill(size, CommitWidth)(false.B)
  val freeListNextVec = VecInit.fill(size, CommitWidth)(0.U(PhyRegIdxWidth.W))
  val freeListNextValid = VecInit.tabulate(size)(i => freeListNextValidVec(i).reduce(_ | _))
  val freeListNext = VecInit.tabulate(size)(i => Mux(freeListNextValid(i), freeListNextVec(i).reduce(_ | _), freeList(i)))
  when(io.free.valid) {
    io.free.bits.zipWithIndex.foreach { case (v, i) =>
      when(v.valid) {
        val ptr = wptr + PopCount(io.free.bits.map(_.valid).take(i))
        freeListNextVec(ptr.value)(i) := v.bits
        freeListNextValidVec(ptr.value)(i) := true.B
      }
    }
    freeList := freeListNext
    freeListNextValid.zipWithIndex.foreach { case (v, i) => assert(PopCount(v) <= 1.B, s"multiply free phy register $i") }
  }
  Module(new FreeListCheck(size)).io := io
}

class FreeListCheck(size: Int)(implicit p: Parameter) extends CoreModule {
  val io = IO(Input(new FreeListIO))
  val freeList = RegInit(VecInit.tabulate(size)(i => (i + NRArchRegs).U(PhyRegIdxWidth.W)))
  val ptrWidth = log2Up(size)
  val rptr = RegInit(0.U(ptrWidth.W))
  val wptr = RegInit((NRPhyRegs - NRArchRegs).U(ptrWidth.W))
  val refOut = VecInit.fill(RenameWidth)(0.U(PhyRegIdxWidth.W))
  when(io.free.valid) {
    val wptrNext = wptr + PopCount(io.free.bits.map(_.valid))
    wptr := Mux(wptrNext < size.U, wptrNext, wptrNext - size.U)
    io.free.bits.zipWithIndex.foreach { case (v, i) =>
      val ptr = wptr + PopCount(io.free.bits.map(_.valid).take(i))
      val addr = Mux(ptr < size.U, ptr, ptr - size.U)
      when(v.valid) {
        freeList(addr) := v.bits
      }
    }
  }
  when(io.allocate.req.fire) {
    val rptrNext = rptr + PopCount(io.allocate.req.bits)
    rptr := Mux(rptrNext < size.U, rptrNext, rptrNext - size.U)
    io.allocate.req.bits.zipWithIndex.foreach { case (v, i) =>
      val ptr = rptr + PopCount(io.allocate.req.bits.take(i))
      val addr = Mux(ptr < size.U, ptr, ptr - size.U)
      when(v) {
        refOut(i) := freeList(addr)
        assert(refOut(i) === io.allocate.rsp(i), s"allocate rsp port $i error")
      }
    }
  }

}

object FreeList extends App {
  implicit val p = new Parameter
  ChiselStage.emitSystemVerilogFile(new FreeList(p.NRPhyRegs))
}
