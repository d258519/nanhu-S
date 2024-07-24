package core.backend.rob

import chisel3._
import chisel3.util._
import core._

class RobCommitIO(implicit p: Parameter) extends CoreBundle {
  val op = Vec(CommitWidth, ValidIO(new MicroOP()))
  val walk = Output(Bool())
  val commit = Output(Bool())
}

class rob(implicit p: Parameter) extends CoreModule {

}
