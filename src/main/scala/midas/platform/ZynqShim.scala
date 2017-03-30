package midas
package platform

import util.ParameterizedBundle // from rocketchip

import chisel3._
import chisel3.util._
import junctions._
import cde.{Parameters, Field}

trait CatapultIFParams {
  val PCIE_DATA_WIDTH = 128
  val PCIE_SLOT_WIDTH = 16
  val PCIE_PAD_WIDTH = 4

  // the document says 32, but the code says 64...
  val UMI_ADDR_WIDTH = 64

  // looks like use_ecc_ddr is hardcoded to 1? in
  // Shells/Academic/shell_parameters.tcl
  //
  // the document says 512..
  val UMI_DATA_WIDTH = 576

  val SOFTREG_ADDR_WIDTH = 32
  val SOFTREG_DATA_WIDTH = 64
}

class CatapultMemReq extends Bundle with CatapultIFParams {
  // OMIT VALID HERE
  // should wrap with ready/valid
  val isWrite = Bool()
  val addr = UInt(width=UMI_ADDR_WIDTH)
  val data = UInt(width=UMI_DATA_WIDTH)
}

class CatapultMemResp extends Bundle with CatapultIFParams {
  // OMIT VALID HERE
  // should wrap with ready/valid
  val data = UInt(width=UMI_DATA_WIDTH)
}

class SimUMIMem(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new CatapultMemReq))
    val resp = Decoupled(new CatapultMemResp)
  })

  val umiDataWidth = 512
  val umiOffsetBits = log2Ceil(umiDataWidth)
  val size = 4L << 30L
  val depth = size >> umiOffsetBits

  val mem = Mem(depth.intValue, UInt(width=umiDataWidth))

  val req = Queue(io.req, 1)
  val idx = req.bits.addr >> umiOffsetBits

  req.ready := req.bits.isWrite || io.resp.ready
  io.resp.valid := req.valid && !req.bits.isWrite
  io.resp.bits.data := mem(idx)

  when (req.fire() && req.bits.isWrite) {
    mem(idx) := req.bits.data
  }
}

/* todo move to firesim */
class NastiUMIAdapter(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val nastimem = Flipped(new NastiIO)
		
		val umireq = Decoupled(new CatapultMemReq)
		val umiresp = Flipped(Decoupled(new CatapultMemResp))
  })

  // hide everything behind queues
  val awQ = Module(new Queue(new NastiWriteAddressChannel, 2))
  val wQ = Module(new Queue(new NastiWriteDataChannel, 2))
  val bQ = Module(new Queue(new NastiWriteResponseChannel, 2))
  val arQ = Module(new Queue(new NastiReadAddressChannel, 2))
  val rQ = Module(new Queue(new NastiReadDataChannel, 2))

  val umireqQ = Module(new Queue(new CatapultMemReq, 2))
  val umirespQ = Module(new Queue(new CatapultMemResp, 2))

  awQ.io.enq <> io.nastimem.aw
  wQ.io.enq <> io.nastimem.w
  io.nastimem.b <> bQ.io.deq
  arQ.io.enq <> io.nastimem.ar
  io.nastimem.r <> rQ.io.deq

  io.umireq <> umireqQ.io.deq
  umirespQ.io.enq <> io.umiresp

  // TODO: 
  // awQ.io.deq  // write addr IN
  // wQ.io.deq   // write data IN
  // bQ.io.enq   // write resp OUT
  //
  // arQ.io.deq // read addr IN
  // rQ.io.enq  // read resp OUT
  //
  // umireqQ.io.enq // umi requests OUT
  // umirespQ.io.deq // umi resps IN



  def fire_writereq(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      awQ.io.deq.valid,
      wQ.io.deq.valid,
      bQ.io.enq.ready,
      umireqQ.io.enq.ready
    )
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  // keep track of:
//  awQ.io.deq.valid & wQ.io.deq.valid & bQ.io.enq.ready & umireqQ.io.enq.ready
  awQ.io.deq.ready := fire_writereq(awQ.io.deq.valid)
  wQ.io.deq.ready := fire_writereq(wQ.io.deq.valid)
  bQ.io.enq.valid := fire_writereq(bQ.io.enq.ready)
  umireqQ.io.enq.valid := fire_writereq(umireqQ.io.enq.ready)
 
  // lower 6 bits must be zero since we're faking a 512 bit block
  umireqQ.io.enq.bits.addr := awQ.io.deq.bits.addr << UInt(3)
  umireqQ.io.enq.bits.data := wQ.io.deq.bits.data
  umireqQ.io.enq.bits.isWrite := UInt(1) // TODO later

  bQ.io.enq.bits.id := awQ.io.deq.bits.id
  bQ.io.enq.bits.resp := UInt(0) // TODO
  bQ.io.enq.bits.user := UInt(0) // TODO





  // TODO: stick in an assert for strb
  arQ.io.deq.ready := UInt(1)
  rQ.io.enq.valid := UInt(0)

}

abstract class PlatformShim extends Module {
  def top: midas.core.FPGATop
  def headerConsts: Seq[(String, Int)]
  def genHeader(sb: StringBuilder, target: String) {
    import widgets.CppGenerationUtils._
    sb.append(genStatic("TARGET_NAME", widgets.CStrLit(target)))
    sb.append(genMacro("PLATFORM_TYPE", s"V${this.getClass.getSimpleName}"))
    if (top.sim.enableSnapshot) sb append(genMacro("ENABLE_SNAPSHOT"))
    sb.append(genMacro("data_t", "uint%d_t".format(top.sim.channelWidth)))
    top.genHeader(sb)(top.sim.channelWidth)
    sb.append("\n// Simulation Constants\n")
    headerConsts map { case (name, value) =>
      genMacro(name, widgets.UInt32(value)) } addString sb
  }
}

case object MasterNastiKey extends Field[NastiParameters]
case object SlaveNastiKey extends Field[NastiParameters]

class ZynqShimIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val master = Flipped(new NastiIO()(p alter Map(NastiKey -> p(MasterNastiKey))))
  val slave  = new NastiIO()(p alter Map(NastiKey -> p(SlaveNastiKey)))
}

class ZynqShim(simIo: midas.core.SimWrapperIO)
              (implicit p: Parameters) extends PlatformShim {
  val io = IO(new ZynqShimIO)
  val top = Module(new midas.core.FPGATop(simIo))
  val headerConsts = List(
    "MMIO_WIDTH" -> p(MasterNastiKey).dataBits / 8,
    "MEM_WIDTH"  -> p(SlaveNastiKey).dataBits / 8
  ) ++ top.headerConsts

  top.io.ctrl <> io.master

  val nastiumi = Module(new NastiUMIAdapter()(  p alter Map(NastiKey -> p(SlaveNastiKey))     ))
  val simumimem = Module(new SimUMIMem)
  
  nastiumi.io.nastimem <> top.io.mem
  simumimem.io.req <> nastiumi.io.umireq
  nastiumi.io.umiresp <> simumimem.io.resp
  // disconnected from top level 
  //io.slave <> top.io.mem
  // tie off top level
  io.slave.ar.valid := UInt(0)
  io.slave.aw.valid := UInt(0)
  io.slave.w.valid := UInt(0)
}
