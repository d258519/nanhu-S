import chisel3._
import circt.stage.ChiselStage


class Counter extends Module {
  val io = IO(new Bundle {
    val led = Output(UInt(1.W))
  })
  val CNT_MAX = 1000.U
  val cnt = RegInit(0.U(32.W))
  val out = RegInit(0.U(1.W))


  cnt := Mux(cnt === CNT_MAX - 1.U, 0.U, cnt + 1.U)
  out := Mux(cnt === CNT_MAX - 1.U, !out, out);

  io.led := out

}
object Counter extends App{
  ChiselStage.emitSystemVerilogFile(new Counter)

}
