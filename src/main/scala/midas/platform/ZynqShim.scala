package midas
package platform

import freechips.rocketchip.util.ParameterizedBundle // from rocketchip

import chisel3._
import chisel3.util._
import junctions._
import freechips.rocketchip.config.{Parameters, Field}

abstract class PlatformShim(implicit p: Parameters) extends Module {
  def top: midas.core.FPGATop
  def headerConsts: Seq[(String, Int)]
  def genHeader(sb: StringBuilder, target: String) {
    import widgets.CppGenerationUtils._
    sb.append(genStatic("TARGET_NAME", widgets.CStrLit(target)))
    sb.append(genMacro("PLATFORM_TYPE", s"V${this.getClass.getSimpleName}"))
    if (p(EnableSnapshot)) {
      sb append(genMacro("ENABLE_SNAPSHOT"))
      if (p(KeepSamplesInMem)) sb append(genMacro("KEEP_SAMPLES_IN_MEM"))
    }
    sb.append(genMacro("data_t", "uint%d_t".format(top.sim.channelWidth)))
    top.genHeader(sb)(top.sim.channelWidth)
    sb.append("\n// Simulation Constants\n")
    headerConsts map { case (name, value) =>
      genMacro(name, widgets.UInt32(value)) } addString sb
  }
}

class ZynqShimIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val master = Flipped(widgets.WidgetMMIO())
  val slave  = new NastiIO()(p alterPartial ({ case NastiKey => p(midas.core.MemNastiKey) }))
}

class ZynqShim(simIo: midas.core.SimWrapperIO)
              (implicit p: Parameters) extends PlatformShim {
  val io = IO(new ZynqShimIO)
  val top = Module(new midas.core.FPGATop(simIo))
  val headerConsts = top.headerConsts

  top.io.mmio <> io.master
  io.slave <> top.io.mem
}
