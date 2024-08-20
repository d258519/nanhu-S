package utils


import chisel3._
import chisel3.util._

import chisel3.experimental._


object AdditionalResource {
  def apply(resource: String) = {
    Module(new ExtModule() with HasExtModuleResource with HasExtModuleInline {
      addResource(resource)
      setInline("Resource_Anon.v",
        s"""
          |module Resource_Anon;
          |endmodule
          |
          |""".stripMargin)
    })
    class Resource_Anon extends Module {
      val io = IO(new Bundle {})
    }
    Module(new Resource_Anon)
  }
}
