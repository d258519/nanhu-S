package npc

import chisel3._
import chisel3.util._

case class Parameter
(
  XLEN: Int = 64,
  NRArchRegs: Int = 32,
  PCBits: Int = 32,
  RenameWidth: Int = 6,
  NRPhyRegs: Int = 192,
)

trait HasCoreParameter {
  implicit val p: Parameter
  val XLEN = p.XLEN
  val NRArchRegs = p.NRArchRegs
  val PCBits = p.PCBits
  val RenameWidth = p.RenameWidth
  val NRPhyRegs = p.NRPhyRegs


  val IMMBits = 32
  val ArchRegIdxWidth = log2Up(NRArchRegs)
  val PhyRegIdxWidth = log2Up(NRPhyRegs)
}

abstract class CoreModule(implicit val p: Parameter) extends Module with HasCoreParameter

abstract class CoreBundle(implicit val p: Parameter) extends Bundle with HasCoreParameter
