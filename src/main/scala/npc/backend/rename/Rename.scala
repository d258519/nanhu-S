package npc.backend.rename

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import npc._

class Rename(implicit p: Parameter) extends CoreModule {
  val io = IO(new Bundle() {
    val in = Vec(RenameWidth, Flipped(DecoupledIO(new MacroOP)))
    val out = Vec(RenameWidth,Flipped(DecoupledIO(new MicroOP)))
  })

//  for (i <- 0 until RenameWidth) {
//    io.out(i).bits := DontCare
//  }
  io.out := DontCare
  io.in := DontCare
}

object Rename extends App {
  implicit val p = new Parameter
  ChiselStage.emitSystemVerilogFile(new Rename)

}