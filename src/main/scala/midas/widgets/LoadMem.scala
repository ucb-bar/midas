// See LICENSE for license details.

package midas
package widgets

import Chisel._
import junctions._
import midas.util.MultiWidthFifo
import freechips.rocketchip.config.{Parameters, Field}

import scala.math.max

class LoadMemIO(hKey: Field[NastiParameters])(implicit p: Parameters) extends WidgetIO()(p){
  // TODO: Slave nasti key should be passed in explicitly
  val toSlaveMem = new NastiIO()(p alterPartial ({ case NastiKey => p(hKey) }))
}

class NastiParams()(implicit val p: Parameters) extends HasNastiParameters

// A crude load mem unit that writes in single beats into the destination memory system
// Arguments:
//  Hkey -> the Nasti key for the interconnect of the memory system we are writing to
class LoadMemWidget(hKey: Field[NastiParameters])(implicit p: Parameters) extends Widget()(p) {
  val io = IO(new LoadMemIO(hKey))

  // prefix h -> host memory we are writing to
  // prefix c -> control nasti interface who is the master of this unit
  val hParams = new NastiParams()(p alterPartial ({ case NastiKey => p(hKey) }))
  val cParams = new NastiParams()(p alterPartial ({ case NastiKey => p(CtrlNastiKey) }))

  val cWidth = p(CtrlNastiKey).dataBits
  val hWidth = p(hKey).dataBits
  val size = hParams.bytesToXSize(UInt(hWidth/8))
  val widthRatio = hWidth/cWidth
  require(hWidth >= cWidth)
  require(p(hKey).addrBits <= 2 * cWidth)

  val wAddrH = genWOReg(Wire(UInt(max(0, p(hKey).addrBits - 32).W)), "W_ADDRESS_H")
  val wAddrQ = genAndAttachQueue(Wire(Decoupled(UInt(p(hKey).addrBits.W))), "W_ADDRESS_L")
  io.toSlaveMem.aw.bits := NastiWriteAddressChannel(
      id = UInt(0),
      addr = Cat(wAddrH, wAddrQ.bits),
      size = size)(p alterPartial ({ case NastiKey => p(hKey) }))
  io.toSlaveMem.aw.valid := wAddrQ.valid
  wAddrQ.ready := io.toSlaveMem.aw.ready

  val wDataQ = Module(new MultiWidthFifo(cWidth, hWidth, 2))
  attachDecoupledSink(wDataQ.io.in, "W_DATA")

  io.toSlaveMem.w.bits := NastiWriteDataChannel(data = wDataQ.io.out.bits)(
      p alterPartial ({ case NastiKey => p(hKey) }))
  io.toSlaveMem.w.valid := wDataQ.io.out.valid
  wDataQ.io.out.ready := io.toSlaveMem.w.ready

  // TODO: Handle write responses better?
  io.toSlaveMem.b.ready := Bool(true)

  val rAddrH = genWOReg(Wire(UInt(max(0, p(hKey).addrBits - 32).W)), "R_ADDRESS_H")
  val rAddrQ = genAndAttachQueue(Wire(Decoupled(UInt(p(hKey).addrBits.W))), "R_ADDRESS_L")
  io.toSlaveMem.ar.bits := NastiReadAddressChannel(
      id = UInt(0),
      addr = Cat(rAddrH, rAddrQ.bits),
      size = size)(p alterPartial ({ case NastiKey => p(hKey) }))
  io.toSlaveMem.ar.valid := rAddrQ.valid
  rAddrQ.ready := io.toSlaveMem.ar.ready

  val rDataQ = Module(new MultiWidthFifo(hWidth, cWidth, 2))
  attachDecoupledSource(rDataQ.io.out, "R_DATA")
  io.toSlaveMem.r.ready := rDataQ.io.in.ready
  rDataQ.io.in.valid := io.toSlaveMem.r.valid

  genCRFile()

  override def genHeader(base: BigInt, sb: StringBuilder) {
    super.genHeader(base, sb)
    import CppGenerationUtils._
    sb.append(genMacro("MEM_DATA_CHUNK", UInt64(
      (p(hKey).dataBits - 1) / p(midas.core.ChannelWidth) + 1)))
  }
}
