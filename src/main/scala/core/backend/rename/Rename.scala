package core.backend.rename

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import core._
import core.backend.rob.RobCommitIO


class Rename(implicit p: Parameter) extends CoreModule {
  val io = IO(new Bundle() {
    val in = Vec(RenameWidth, Flipped(DecoupledIO(new MacroOP)))
    val out = Vec(RenameWidth, DecoupledIO(new MicroOP))
    val commit = Flipped(new RobCommitIO())

  })

  io.out := DontCare
  io.in := DontCare
}
