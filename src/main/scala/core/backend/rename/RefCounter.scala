package core.backend.rename

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import core._
import core.general._


class RefCounter(implicit p: Parameter) extends CoreModule {
  val io = IO(new Bundle {
    val allocate = Flipped(Vec(RenameWidth, ValidIO(UInt(PhyRegIdxWidth.W))))
    val in = Flipped(ValidIO(Vec(CommitWidth, ValidIO(UInt(PhyRegIdxWidth.W)))))
    val out = ValidIO(Vec(CommitWidth, ValidIO(UInt(PhyRegIdxWidth.W))))
  })
  val reqOH = io.allocate.map(r => UIntToOH(r.bits, NRPhyRegs))
  val freeOH = io.in.bits.map(f => UIntToOH(f.bits, NRPhyRegs))
  val refCounter = RegInit(VecInit.tabulate(NRPhyRegs)(i => (if (i < NRArchRegs) 0 else 1).U(log2Up(RobSize + 1).W)))
  val refCounterInc = VecInit.tabulate(NRPhyRegs)(i => PopCount(io.allocate.zipWithIndex.map { case (r, port) => r.valid && reqOH(port)(i) }))
  val refCounterDec = VecInit.tabulate(NRPhyRegs)(i => Mux(io.in.valid, PopCount(io.in.bits.zipWithIndex.map { case (r, port) => r.valid && freeOH(port)(i) }), 0.U))
  val refCounterNext = VecInit.tabulate(NRPhyRegs)(i => refCounter(i) + refCounterInc(i) - refCounterDec(i))
  refCounter := refCounterNext
  // to FreeList
  val reallyFree = VecInit(io.in.bits.map(v => v.valid && refCounterNext(v.bits) === 0.U && refCounter(v.bits) =/= 0.U))
  io.out.valid := RegNext(io.in.valid && reallyFree.asUInt.orR)
  io.out.bits.zipWithIndex.foreach { case (out, i) =>
    // ensure unique freeing of PHY registers per cycle
    val isMultiFree = if (i == 0) false.B else VecInit(io.in.bits.take(i).map(in => in.valid && in.bits === out.bits)).asUInt.orR
    out.valid := RegNext(reallyFree(i) && !isMultiFree)
    out.bits := io.in.bits(i).bits
  }
}

object RefCounter extends App {
  implicit val p = new Parameter
  ChiselStage.emitSystemVerilogFile(new RefCounter())
}
