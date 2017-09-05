package midas
package widgets

import midas.core.NumAsserts
import chisel3._
import chisel3.util._
import config.Parameters

class AssertWidgetIO(implicit p: Parameters) extends WidgetIO()(p) {
  val tReset = Flipped(Decoupled(Bool()))
  val assert = Flipped(Decoupled(UInt((log2Ceil(p(NumAsserts)) + 1).W)))
}

class AssertWidget(implicit p: Parameters) extends Widget()(p) with HasChannels {
  val io = IO(new AssertWidgetIO)
  val resume = Wire(init=false.B)
  val assertId = io.assert.bits >> 1
  val assertFire = io.assert.bits(0) && !io.tReset.bits
  val fire = io.assert.valid && io.tReset.valid && (!assertFire || resume)
  val cycles = Reg(UInt(64.W))
  io.assert.ready := fire
  io.tReset.ready := fire
  when (fire) {
    cycles := Mux(io.tReset.bits, 0.U, cycles + 1.U)
  }

  genROReg(assertId, "id")
  genROReg(assertFire, "fire")
  // FIXME: no hardcode
  genROReg(cycles(31, 0), "cycle_low")
  genROReg(cycles >> 32, "cycle_high")
  Pulsify(genWORegInit(resume, "resume", Bool(false)), pulseLength = 1)
  genCRFile()
}
