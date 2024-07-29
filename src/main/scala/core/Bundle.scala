package core

import chisel3._
import chisel3.util._


object SrcType extends ChiselEnum {
  val register, pc, imm = Value
}

object FuType extends ChiselEnum {
  val ALU, BRU, LSU, MDU, MISC = Value
}

object FuOP extends ChiselEnum {
  val add = Value
}

class InstrEntry(implicit p: Parameter) extends CoreBundle {
  val instr = UInt(32.W)
  val pc = UInt(PCBits.W)
}

class DecodeOp(implicit p: Parameter) extends CoreBundle {
  val instr = new InstrEntry()
  val fuType = FuType()
  val fuOp = FuOP()
  val srcType = Vec(2, SrcType())
  val lsrc = Vec(2, UInt(ArchRegIdxWidth.W))
  val ldst = UInt(ArchRegIdxWidth.W)
  val ldstValid = Bool()
  val imm = UInt(IMMBits.W)
  val isMove = Bool()
}

class MicroOp(implicit p: Parameter) extends CoreBundle {
  val uop = new DecodeOp()
  val psrc = Vec(2, UInt(PhyRegIdxWidth.W))
  val pdst = UInt(PhyRegIdxWidth.W)
  val pdstOld = UInt(PhyRegIdxWidth.W)
  val pdstValid = Bool()
}

class Redirect(implicit p: Parameter) extends CoreBundle {
  val pc = UInt(PCBits.W)
  val op = new MicroOp()
}
