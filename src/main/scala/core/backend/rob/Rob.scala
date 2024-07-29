package core.backend.rob

import chisel3._
import chisel3.util._
import core._

class RobCommitIO(implicit p: Parameter) extends CoreBundle {
  val valid = Output(Bool())
  val isWalk = Output(Bool())
  val op = Vec(CommitWidth, ValidIO(new MicroOp()))
}

class rob(implicit p: Parameter) extends CoreModule {

}
