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

  val umiDataWidth = 512/8
  val umiOffsetBits = log2Ceil(umiDataWidth)
  val size = 2L << 28L
  val depth = size >> umiOffsetBits

  val mem = Mem(depth.intValue, UInt(width=umiDataWidth*8))

  val req = Queue(io.req, 1)
  val idx = req.bits.addr >> umiOffsetBits

  req.ready := req.bits.isWrite || io.resp.ready
  io.resp.valid := req.valid && !req.bits.isWrite
  io.resp.bits.data := mem(idx)

  when (req.fire() && req.bits.isWrite) {
    mem(idx) := req.bits.data
  }
}

/* this takes an axi read request and duplicates it (len + 1) times 
 * for convenience in later stages
 */
class NastiRequestSplitter(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val read_requests = Flipped(Decoupled(new NastiReadAddressChannel))
    val split_read_requests = Decoupled(new NastiReadAddressChannel)
  })

  val splitcountReg = Reg(init=UInt(0, width=32))
  val splitcountRegLast = io.read_requests.bits.len === splitcountReg
  io.split_read_requests.bits := io.read_requests.bits
  io.split_read_requests.bits.addr := io.read_requests.bits.addr + (splitcountReg << UInt(3))
  io.split_read_requests.bits.len := UInt(0) // splitting
  io.split_read_requests.bits.user := splitcountRegLast // HACKY way to pass through last signal

  // read_requests.valid, split_read_requests.ready, splitcountRegLast
  io.read_requests.ready := io.split_read_requests.ready & splitcountRegLast
  io.split_read_requests.valid := io.read_requests.valid

  val incremented_or_plain = Mux(io.read_requests.valid & io.split_read_requests.ready, splitcountReg + UInt(1), splitcountReg)
  splitcountReg := Mux(io.read_requests.valid & io.split_read_requests.ready & splitcountRegLast, UInt(0), incremented_or_plain)
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

  val umireqQwrite = Module(new Queue(new CatapultMemReq, 2))
  val umireqQread = Module(new Queue(new CatapultMemReq, 2))
  val umireqQ = Module(new Queue(new CatapultMemReq, 2))
  val umirespQ = Module(new Queue(new CatapultMemResp, 2))

  awQ.io.enq <> io.nastimem.aw
  wQ.io.enq <> io.nastimem.w
  io.nastimem.b <> bQ.io.deq
  arQ.io.enq <> io.nastimem.ar
  io.nastimem.r <> rQ.io.deq

  io.umireq <> umireqQ.io.deq
  umirespQ.io.enq <> io.umiresp

  // arbitrate between read/write requests for output port
  val umirequestArbiter = Module(new RRArbiter(new CatapultMemReq, 2))
  umireqQ.io.enq <> umirequestArbiter.io.out
  umirequestArbiter.io.in(0) <> umireqQwrite.io.deq
  umirequestArbiter.io.in(1) <> umireqQread.io.deq


  // TODO: 
  // awQ.io.deq  // write addr IN
  // wQ.io.deq   // write data IN
  // bQ.io.enq   // write resp OUT
  //
  // arQ.io.deq // read addr IN
  // rQ.io.enq  // read resp OUT
  //
  // umireqQwrite.io.enq // umi requests OUT
  // umirespQ.io.deq // umi resps IN

  // handle writes
  def fire_writereq(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      awQ.io.deq.valid,
      wQ.io.deq.valid,
      bQ.io.enq.ready,
      umireqQwrite.io.enq.ready
    )
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  // subtract DRAM base from addresses before sending to UMI
  val DRAM_BASE = UInt(BigInt("80000000", 16))

  when (fire_writereq(null)) {
    printf("got write value: %x from address 0x%x\n", wQ.io.deq.bits.data, awQ.io.deq.bits.addr)
  }

  // keep track of:
//  awQ.io.deq.valid & wQ.io.deq.valid & bQ.io.enq.ready & umireqQwrite.io.enq.ready
  awQ.io.deq.ready := fire_writereq(awQ.io.deq.valid)
  wQ.io.deq.ready := fire_writereq(wQ.io.deq.valid)
  bQ.io.enq.valid := fire_writereq(bQ.io.enq.ready)
  umireqQwrite.io.enq.valid := fire_writereq(umireqQwrite.io.enq.ready)
 
  // lower 6 bits must be zero since we're faking a 512 bit block
  umireqQwrite.io.enq.bits.addr := (awQ.io.deq.bits.addr - DRAM_BASE) << UInt(3)
  umireqQwrite.io.enq.bits.data := wQ.io.deq.bits.data
  umireqQwrite.io.enq.bits.isWrite := UInt(1)
  bQ.io.enq.bits.id := awQ.io.deq.bits.id
  bQ.io.enq.bits.resp := UInt(0) // TODO
  bQ.io.enq.bits.user := UInt(0) // TODO

  // --------------------------------------------------------------------
  // handle reads
  //
  // we have: umireqQread.io.enq
  // umirespQ.io.deq
  //
  // arQ.io.deq
  // rQ.io.enq
  val arsplitQ = Module(new Queue(new NastiReadAddressChannel, 20))
  val splitter = Module(new NastiRequestSplitter())
  splitter.io.read_requests <> arQ.io.deq
  arsplitQ.io.enq <> splitter.io.split_read_requests
  val readInFlightQ = Module(new Queue(new NastiReadAddressChannel, 10))

  // two steps:
  // sending requests:
  // care about:
  //  readInFlightQ.io.enq
  //  arsplitQ.io.deq
  //  umireqQread.io.enq


  def fire_read_stage1(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      readInFlightQ.io.enq.ready,
      arsplitQ.io.deq.valid,
      umireqQread.io.enq.ready
    )
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }
  readInFlightQ.io.enq.valid := fire_read_stage1(readInFlightQ.io.enq.ready)
  arsplitQ.io.deq.ready := fire_read_stage1(arsplitQ.io.deq.valid)
  umireqQread.io.enq.valid := fire_read_stage1(umireqQread.io.enq.ready)

  readInFlightQ.io.enq.bits := arsplitQ.io.deq.bits
  umireqQread.io.enq.bits.addr := (arsplitQ.io.deq.bits.addr - DRAM_BASE) << UInt(3)
  umireqQread.io.enq.bits.data := UInt(0)
  umireqQread.io.enq.bits.isWrite := UInt(0)

  // second step:
  // getting responses:
  // care about:
  //  readInFlightQ.io.deq
  //  rQ.io.enq
  //  umirespQ.io.deq

  def fire_read_stage2(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      readInFlightQ.io.deq.valid,
      rQ.io.enq.ready,
      umirespQ.io.deq.valid
    )
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  when(fire_read_stage2(null)) {
    printf("got read value: %x from address 0x%x\n", rQ.io.enq.bits.data, readInFlightQ.io.deq.bits.addr)
  }


  readInFlightQ.io.deq.ready := fire_read_stage2(readInFlightQ.io.deq.valid)
  rQ.io.enq.valid := fire_read_stage2(rQ.io.enq.ready)
  umirespQ.io.deq.ready := fire_read_stage2(umirespQ.io.deq.valid)

  rQ.io.enq.bits.id := readInFlightQ.io.deq.bits.id
  rQ.io.enq.bits.data := umirespQ.io.deq.bits.data(63,0)
  rQ.io.enq.bits.last := readInFlightQ.io.deq.bits.user
  rQ.io.enq.bits.resp := UInt(0)
  rQ.io.enq.bits.user := UInt(0)

  // TODO: stick in an assert for strb
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
  // tie off top level
  io.slave.ar.valid := UInt(0)
  io.slave.aw.valid := UInt(0)
  io.slave.w.valid := UInt(0)


  // disconnected from top level 
//  io.slave <> top.io.mem

}
