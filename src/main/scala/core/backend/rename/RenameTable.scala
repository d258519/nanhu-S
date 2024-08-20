package core.backend.rename

import _root_.circt.stage.ChiselStage
import chisel3._
import core._

class RatReadIO(implicit p: Parameter) extends CoreBundle {
  val addr = Input(UInt(ArchRegIdxWidth.W))
  val data = Output(UInt(PhyRegIdxWidth.W))
}

class RatWriteIO(implicit p: Parameter) extends CoreBundle {
  val valid = Input(Bool())
  val addr = Input(UInt(ArchRegIdxWidth.W))
  val data = Input(UInt(PhyRegIdxWidth.W))
}

class RenameTable(implicit p: Parameter) extends CoreModule {
  val io = IO(new Bundle() {
    val read = Vec(RenameWidth, Vec(3, new RatReadIO()))
    val write = Vec(RenameWidth.max(CommitWidth), new RatWriteIO())
  })

  val tableInit = VecInit.tabulate(NRArchRegs)(_.U(PhyRegIdxWidth.W))
  val table = RegInit(tableInit)
  val tableNext = WireInit(table)
  table := tableNext
  io.read.flatten.foreach(r => r.data := RegNext(tableNext(r.addr)))
  /* The write requests by ROB walking have priority. The older instructions have higher priority. */
  io.write.foreach(w => when(w.valid) {
    tableNext(w.addr) := w.data
  })
}
