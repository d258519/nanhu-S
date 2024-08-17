package core.backend.rename

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import core._
import core.backend.rob.RobCommitIO


class Rename(implicit p: Parameter) extends CoreModule {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(Vec(RenameWidth, ValidIO(new DecodeOp))))
    val out = DecoupledIO(Vec(RenameWidth, ValidIO(new MicroOp)))
    val commit = Flipped(new RobCommitIO)
    val redirect = Flipped(ValidIO(new Redirect))
    val intRatWrite = Flipped(Vec(RenameWidth, new RatWriteIO))
    val intRatRead = Input(Vec(RenameWidth, Vec(3, new RatReadIO())))
  })
  val intFreeList = Module(new FreeList(NRPhyRegs))
  val intRefCounter = Module(new RefCounter)

  val outReady = io.out.ready || !io.out.valid

  for (i <- 0 until RenameWidth) {
    val uop = io.in.bits(i).bits
    val mop = Wire(new MicroOp)
    val needAllocate = io.in.valid && io.in.bits(i).valid && uop.ldstValid
    val isMove = uop.isMove
    mop.uop := uop
    mop.pdstValid := uop.ldstValid & !isMove
    mop.pdst := intFreeList.io.allocate.rsp(i)

    io.intRatWrite(i).valid := io.in.fire && needAllocate
    io.intRatWrite(i).addr := uop.ldst
    io.intRatWrite(i).data := Mux(isMove, mop.psrc(0), mop.pdst)
    // rename table read bypass to micro op
    (mop.psrc ++ Seq(mop.pdstOld)).zipWithIndex.foreach { case (phyReg, idx) =>
      phyReg := io.intRatRead(i)(idx).data
      io.intRatWrite.take(i).foreach(v => when(v.valid && v.addr === uop.lsrc.concat(Seq(uop.ldst))(idx))(phyReg := v.data))
    }

    // lui to load fusion
    // generate last uop
    val (lastUop, lastUopValid) = if (i > 0) {
      (io.in.bits(i - 1).bits, io.in.bits(i - 1).valid)
    } else {
      val inOpCount = PopCount(io.in.bits.map(_.valid))
      val lastOp = io.in.bits(inOpCount - 1.U)
      (RegEnable(lastOp.bits, io.in.fire && inOpCount =/= 0.U),
        RegEnable(lastOp.valid, false.B, io.in.fire && inOpCount =/= 0.U))
    }
    // replace imm and modify srcType(0）to zero
    // before fusion
    //    lui x1, 0x123
    //    ld x1, 0x456
    // after fusion
    //    lui x1, 0x123
    //    ld x0, 0x123456
    when(lastUopValid && lastUop.isLui && uop.isLoad && lastUop.ldst === uop.lsrc(0)) {
      mop.uop.imm := Cat(lastUop.imm(IMMBits - 1, 12), uop.imm(11, 0))
      mop.uop.srcType(0) := SrcType.ZERO
    }

    intFreeList.io.allocate.req.bits(i) := needAllocate && !isMove

    intRefCounter.io.allocate(i).bits := mop.pdst
    intRefCounter.io.allocate(i).valid := io.in.fire && needAllocate

    io.out.bits(i).valid := RegEnable(io.in.bits(i).valid, false.B, io.in.fire)
    io.out.bits(i).bits := RegEnable(mop, io.in.fire)
  }

  for (i <- 0 until CommitWidth) {
    val commitOp = io.commit.op(i)
    intRefCounter.io.in.bits(i).valid := commitOp.valid && commitOp.bits.pdstValid
    intRefCounter.io.in.bits(i).bits := Mux(io.commit.isWalk, commitOp.bits.pdst, commitOp.bits.pdstOld)
  }

  intRefCounter.io.in.valid := io.commit.valid
  intFreeList.io.free := intRefCounter.io.out
  intFreeList.io.allocate.req.valid := io.in.valid && outReady && !io.redirect.valid
  // when redirect signal valid, invalid rename request by pull down the io.in.ready signal.
  // the renamed mop should not be flushed directly, due to the rat and freelist have been modified
  val outValidSet = io.in.fire
  val outValidClr = io.out.fire
  io.out.valid := RegEnable(outValidSet, false.B, outValidSet || outValidClr)
  io.in.ready := outReady && intFreeList.io.allocate.req.ready && !io.redirect.valid

}

object Rename extends App {
  implicit val p = new Parameter
  ChiselStage.emitSystemVerilogFile(new Rename)
}
