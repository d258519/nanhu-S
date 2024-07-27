package core.backend.rename

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import core._
import core.general._

class FreeList(size: Int)(implicit p: Parameter) extends CoreModule {
  val io = IO(new Bundle {
    val allocate = new Bundle {
      val req = Flipped(DecoupledIO(Vec(RenameWidth, Bool())))
      val rsp = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
    }
    val free = Flipped(ValidIO(Vec(CommitWidth, ValidIO(UInt(PhyRegIdxWidth.W)))))
  })
  val freeList = RegInit(VecInit.tabulate(size)(i => (i + NRArchRegs).U(PhyRegIdxWidth.W)))
  val zeroPtr = 0.U.asTypeOf(CirclePtr(size))
  val rptrNext, wptrNext = WireInit(zeroPtr)
  val rptr = RegEnable(rptrNext, zeroPtr, io.allocate.req.fire)
  val wptr = RegEnable(wptrNext, (NRPhyRegs - NRArchRegs).U.asTypeOf(zeroPtr), io.free.valid)
  rptrNext := rptr + PopCount(io.allocate.req.bits)
  wptrNext := wptr + PopCount(io.free.bits.map(_.valid))
  val rptrOH = RegEnable(UIntToOH(rptrNext.asUInt, size), 1.U(size.W), io.allocate.req.fire)
  val rptrOHs = VecInit.tabulate(RenameWidth)(i => rptrOH.rotateLeft(PopCount(io.allocate.req.bits.take(i))))
  io.allocate.rsp.zipWithIndex.foreach { case (v, i) => v := Mux1H(rptrOHs(i), freeList) }
  val readEntryNumNext = wptr - rptr
  io.allocate.req.ready := RegNext(readEntryNumNext >= RenameWidth.U, false.B)
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

}

object FreeList extends App {
  implicit val p = new Parameter
  ChiselStage.emitSystemVerilogFile(new FreeList(p.NRPhyRegs))
}
