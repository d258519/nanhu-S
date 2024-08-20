package core.backend.rename

import chisel3._
import chisel3.util._
import core._
import core.backend.rob.RobCommitIO
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import simulation.MSimulator._
import _root_.circt.stage.ChiselStage

import scala.util.Random
import scala.collection._

class RenameWrapper(implicit p: Parameter) extends CoreModule {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec(RenameWidth, ValidIO(new DecodeOp))))
    val out = DecoupledIO(Vec(RenameWidth, ValidIO(new MicroOp)))
    val commit = Flipped(new RobCommitIO)
    val redirect = Flipped(ValidIO(new Redirect))
  })
  val rename = Module(new Rename)
  val rat = Module(new RenameTable)
  for (i <- 0 until RenameWidth.max(CommitWidth)) {
    if (i < RenameWidth) {
      rat.io.write(i) := rename.io.intRatWrite(i)
    } else {
      rat.io.write(i) := 0.U.asTypeOf(new RatWriteIO)
    }
  }
  when(io.commit.valid && io.commit.isWalk) {
    for (i <- 0 until RenameWidth.max(CommitWidth)) {
      if (i < CommitWidth) {
        rat.io.write(i).valid := io.commit.op(i).valid && io.commit.op(i).bits.pdstValid
        rat.io.write(i).addr := io.commit.op(i).bits.uop.ldst
        rat.io.write(i).data := io.commit.op(i).bits.pdstOld
      } else {
        rat.io.write(i) := 0.U.asTypeOf(new RatWriteIO)
      }
    }
  }
  rename.io.intRatRead := rat.io.read
  rename.io.out <> io.out
  rename.io.commit <> io.commit
  rename.io.redirect <> io.redirect

  rename.io.in.bits := RegNext(io.in.bits)
  rename.io.in.valid := RegNext(io.in.valid)
  io.in.ready := rename.io.in.ready

  rat.io.read.zipWithIndex.foreach { case (v, i) =>
    v(0).addr := io.in.bits(i).bits.lsrc(0)
    v(1).addr := io.in.bits(i).bits.lsrc(1)
    v(2).addr := io.in.bits(i).bits.ldst
  }
}

case class Dop(lsrc0: Int, lsrc1: Int, ldst: Int, isMove: Boolean, isLui: Boolean, isLoad: Boolean)

case class Rop(dop: Dop, psrc0: Int, psrc1: Int, pdst: Int, pdstOld: Int, pdstValid: Boolean)

class RenameRefModule(implicit p: Parameter) {
  val freeList = mutable.Queue[Int]()
  val rat = new Array[Int](32)

  reset()

  def reset(): Unit = {
    freeList.clear()
    for (r <- p.NRArchRegs until p.NRPhyRegs) freeList.enqueue(r)
    for (i <- 0 until p.NRArchRegs) rat(i) = i
  }

  def rename(dop: Dop): Rop = {
    assert(freeList.nonEmpty)
    val (psrc0, psrc1) = (rat(dop.lsrc0), rat(dop.lsrc1))
    val pdstOld = rat(dop.ldst)
    val pdst = freeList.front
    val pdstValid = dop.ldst != 0
    if (pdstValid) {
      rat(dop.ldst) = pdst
      println(s"RAT[${dop.ldst}] => ${pdst}")
      freeList.dequeue()
      println(s"FreeList dequeue ${pdst}")
    }
    Rop(dop, psrc0, psrc1, pdst, pdstOld, pdstValid)
  }

  def commit(rop: Rop): Unit = {
    if (rop.pdstValid) {
      freeList.enqueue(rop.pdstOld)
      println(s"FreeList enqueue ${rop.pdstOld}")
    }

  }

  def walk(rop: Rop): Unit = {
    if (rop.pdstValid) {
      freeList.enqueue(rop.pdst)
      println(s"FreeList enqueue ${rop.pdst}")
      println(s"RAT[${rop.dop.ldst}] => ${rop.pdstOld}")
      rat(rop.dop.ldst) = rop.pdstOld
    }
  }

}

class RenameTest extends AnyFreeSpec with Matchers {
  "RenameTest" in {
    implicit val p = Parameter()
    val ref = new RenameRefModule()
    val random = new Random(seed = 0)
    simulate(new RenameWrapper()) { dut =>
      // normal rename test
      val rob = mutable.Queue[(Rop, Int)]()
      val commitLatency = 10
      var cycle = 0
      reset()

      def renameUpdate(cycle: Int): Unit = {
        dut.clock.step(0)
        if (dut.io.out.valid.peekValue().asBigInt.toInt != 0 && dut.io.out.ready.peekValue().asBigInt.toInt != 0) {
          println(s"  Cycle : $cycle")
          for (i <- 0 until p.RenameWidth) {
            if (dut.io.out.bits(i).valid.peekValue().asBigInt.toInt != 0) {
              val rop = readRop(i)
              println(s"Rename [${rop}]")
              val refRop = ref.rename(rop.dop)
              assert(rop == refRop)
              rob.enqueue((rop, cycle))
            }
          }
        }
      }

      def randRename(): Unit = {
        dut.clock.step(0)
        if (dut.io.in.valid.peekValue().asBigInt.toInt == 0) {
          for (i <- 0 to random.nextInt(p.RenameWidth)) {
            makeDecodeOp(i, Dop(random.nextInt(32), random.nextInt(32), random.nextInt(32), false, false, false))
          }
        }
      }

      def commit(cycle: Int): Unit = {
        dut.io.commit.valid.poke(true.B)
        dut.io.commit.isWalk.poke(false.B)
        for (i <- 0 to random.nextInt(p.CommitWidth)) {
          if (rob.nonEmpty) {
            if (cycle > rob.front._2 + commitLatency) {
              val rop = rob.front._1
              dut.io.commit.op(i).valid.poke(true.B)
              dut.io.commit.op(i).bits.pdstValid.poke(rop.pdstValid)
              dut.io.commit.op(i).bits.pdst.poke(rop.pdst)
              dut.io.commit.op(i).bits.pdstOld.poke(rop.pdstOld)
              rob.dequeue()
              println(s"Commit [${rop}] at $i")
              ref.commit(rop)
            }
          } else {
            dut.io.commit.op(i).valid.poke(false.B)
          }
        }
      }

      def walkAll(): Unit = {
        println("walkAll")
        dut.io.redirect.valid.poke(true.B)
        renameUpdate(cycle)
        dut.clock.step(0)
        dut.clock.step()
        cycle = cycle + 1
        clearCommit()
        clearIn()
        dut.io.redirect.valid.poke(false.B)

        while (rob.nonEmpty) {
          dut.io.commit.valid.poke(true.B)
          dut.io.commit.isWalk.poke(true.B)
          for (i <- 0 to random.nextInt(p.CommitWidth)) {
            if (rob.nonEmpty) {
              val rop = rob.last._1
              dut.io.commit.op(i).valid.poke(true.B)
              dut.io.commit.op(i).bits.pdstValid.poke(rop.pdstValid)
              dut.io.commit.op(i).bits.pdst.poke(rop.pdst)
              dut.io.commit.op(i).bits.pdstOld.poke(rop.pdstOld)
              dut.io.commit.op(i).bits.uop.ldst.poke(rop.dop.ldst)
              rob.removeLast()
              println(s"Walk [${rop}]")
              ref.walk(rop)
            }
          }
          dut.clock.step(0)
          dut.clock.step()
          clearCommit()
          cycle = cycle + 1
        }
        clearCommit()
      }

      while (cycle < 10000) {
        randRename()
        commit(cycle)

        renameUpdate(cycle)
        val inReady = dut.io.in.ready.peekValue().asBigInt.toInt
        dut.clock.step(0)
        dut.clock.step()
        cycle = cycle + 1

        if (inReady != 0) {
          clearIn()
        }
        clearCommit()

        if (cycle % 24 == 0) walkAll()
      }


      def readRop(idx: Int): Rop = {
        dut.clock.step(0)
        val op = dut.io.out.bits(idx).bits
        val dop = Dop(op.uop.lsrc(0).peekValue().asBigInt.toInt,
          op.uop.lsrc(1).peekValue().asBigInt.toInt,
          op.uop.ldst.peekValue().asBigInt.toInt,
          op.uop.isMove.peekValue().asBigInt.toInt != 0,
          op.uop.isLui.peekValue().asBigInt.toInt != 0,
          op.uop.isLoad.peekValue().asBigInt.toInt != 0
        )
        val rop = Rop(dop, op.psrc(0).peekValue().asBigInt.toInt,
          op.psrc(1).peekValue().asBigInt.toInt,
          op.pdst.peekValue().asBigInt.toInt,
          op.pdstOld.peekValue().asBigInt.toInt,
          op.pdstValid.peekValue().asBigInt.toInt != 0
        )
        rop
      }

      def reset(): Unit = {
        clearIn()
        clearCommit()
        dut.io.out.ready.poke(true.B)
        dut.io.redirect.valid.poke(false.B)
        dut.reset.poke(true.B)
        dut.clock.step(0)
        dut.clock.step()
        dut.reset.poke(false.B)
        ref.reset()
        rob.clear()
      }

      def clearCommit(): Unit = {
        dut.io.commit.valid.poke(false.B)
        dut.io.commit.op.foreach(_.valid.poke(false.B))
      }

      def clearIn(): Unit = {
        dut.io.in.valid.poke(false.B)
        dut.io.in.bits.foreach(_.valid.poke(false.B))
      }

      def makeDecodeOp(idx: Int, dop: Dop): Unit = {
        dut.io.in.valid.poke(true.B)
        dut.io.in.bits(idx).valid.poke(true.B)
        dut.io.in.bits(idx).bits.lsrc(0).poke(dop.lsrc0)
        dut.io.in.bits(idx).bits.lsrc(1).poke(dop.lsrc1)
        dut.io.in.bits(idx).bits.ldst.poke(dop.ldst)
        dut.io.in.bits(idx).bits.srcType(0).poke(if (dop.lsrc0 == 0) SrcType.ZERO else SrcType.REG)
        dut.io.in.bits(idx).bits.srcType(1).poke(if (dop.lsrc1 == 0) SrcType.ZERO else SrcType.REG)
        dut.io.in.bits(idx).bits.ldstValid.poke((dop.ldst != 0).B)
        dut.io.in.bits(idx).bits.isMove.poke(dop.isMove.B)
        dut.io.in.bits(idx).bits.isLui.poke(dop.isLui.B)
        dut.io.in.bits(idx).bits.isLoad.poke(dop.isLoad.B)
      }


    }
  }
}

