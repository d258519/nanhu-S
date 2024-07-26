package core.backend.rename

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import core._
import core.general._

class FreeListIO(implicit p: Parameter) extends CoreBundle {
  val req = Flipped(DecoupledIO(Vec(RenameWidth, Bool())))
  val rsp = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
  val free = Flipped(ValidIO(Vec(CommitWidth, ValidIO(UInt(PhyRegIdxWidth.W)))))
}

class FreeList(size: Int)(implicit p: Parameter) extends CoreModule {
  val io = IO(new FreeListIO)
  val freeList = RegInit(VecInit.tabulate(size)(i => (i + 32).U(PhyRegIdxWidth.W)))
  val zeroPtr = 0.U.asTypeOf(CirclePtr(size))
  val rptrNext, wptrNext = WireInit(zeroPtr)
  val rptr = RegEnable(rptrNext, zeroPtr, io.req.fire)
  val wptr = RegEnable(wptrNext, (NRPhyRegs - 32).U.asTypeOf(zeroPtr), io.free.valid)
  rptrNext := rptr + PopCount(io.req.bits)
  wptrNext := wptr + PopCount(io.free.bits.map(_.valid))
  val rptrOH = RegEnable(UIntToOH(rptrNext.asUInt, size), 1.U(size.W), io.req.fire)
  val rptrOHs = VecInit.tabulate(RenameWidth)(i => rptrOH.rotateLeft(PopCount(io.req.bits.take(i))))
  io.rsp.zipWithIndex.foreach { case (v, i) => v := Mux1H(rptrOHs(i), freeList) }
  val readEntryNumNext = wptr - rptr
  io.req.ready := RegNext(readEntryNumNext >= RenameWidth.U, false.B)
  val wptrOH = RegEnable(UIntToOH(wptrNext.asUInt, size), 1.U(size.W), io.free.valid)
  val wptrOHs = VecInit.tabulate(CommitWidth)(i => wptrOH.rotateLeft(PopCount(io.free.bits.map(_.valid).take(i))))
  //TODO: convert the freelist write logic from serial to parallel
  when(io.free.valid) {
    io.free.bits.zipWithIndex.foreach { case (v, i) =>
      when(v.valid) {
        freeList(wptrOHs(i)) := v.bits
      }
    }
  }
}

object FreeList extends App {
  implicit val p = new Parameter
  ChiselStage.emitSystemVerilogFile(new FreeList(p.NRPhyRegs))
}
