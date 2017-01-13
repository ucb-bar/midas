package midas
package widgets

// from rocketchip
import junctions._

import chisel3._
import chisel3.util._
import cde.{Parameters, Field}

abstract class EndpointWidgetIO(implicit p: Parameters) extends WidgetIO()(p) {
  def hPort: HostPortIO[Data]
  val tReset = Flipped(Decoupled(Bool()))
}

abstract class EndpointWidget(implicit p: Parameters) extends Widget()(p) {
  override def io: EndpointWidgetIO
}

abstract class MemModelConfig // TODO: delete it

class MemModelIO(implicit p: Parameters) extends EndpointWidgetIO()(p){
  val tNasti = Flipped(HostPort(new NastiIO, false))
  val host_mem = new NastiIO
  def hPort = tNasti
}

abstract class MemModel(implicit p: Parameters) extends EndpointWidget()(p){
  val io = IO(new MemModelIO)
  override def genHeader(base: BigInt, sb: StringBuilder) {
    super.genHeader(base, sb)
    import CppGenerationUtils._
    sb.append(genMacro(this.getClass.getSimpleName))
  }
}

abstract class NastiWidgetBase(implicit p: Parameters) extends MemModel {
  val tNasti = io.tNasti.hBits
  val tReset = io.tReset.bits
  val targetFire = io.tNasti.toHost.hValid && io.tNasti.fromHost.hReady && io.tReset.valid

  val arBuf = Module(new Queue(new NastiReadAddressChannel,   4, flow=true))
  val awBuf = Module(new Queue(new NastiWriteAddressChannel,  4, flow=true))
  val wBuf  = Module(new Queue(new NastiWriteDataChannel,    16, flow=true))
  val rBuf  = Module(new Queue(new NastiReadDataChannel,     16, flow=true))
  val bBuf  = Module(new Queue(new NastiWriteResponseChannel, 4, flow=true))

  def elaborate(stall: Bool,
                rCycleValid: Bool = Bool(true),
                wCycleValid: Bool = Bool(true),
                rCycleReady: Bool = Bool(true),
                wCycleReady: Bool = Bool(true)) = {
    val fire = targetFire && !stall
    fire suggestName "fire"
    io.tNasti.toHost.hReady := fire
    io.tNasti.fromHost.hValid := fire
    io.tReset.ready := fire

    // Bad assumption: We have no outstanding read or write requests to host
    // during target reset. This will be handled properly in the fully fledged
    // memory model; i'm too lazy to properly handle this here.
    val targetReset = fire && tReset
    targetReset suggestName "targetReset"
    arBuf.reset := reset || targetReset
    awBuf.reset := reset || targetReset
    rBuf.reset := reset || targetReset
    bBuf.reset := reset || targetReset
    wBuf.reset := reset || targetReset

    // Request
    tNasti.ar.ready    := arBuf.io.enq.ready && rCycleReady
    tNasti.aw.ready    := awBuf.io.enq.ready && wCycleReady
    tNasti.w.ready     := wBuf.io.enq.ready  && wCycleReady
    arBuf.io.enq.valid := tNasti.ar.valid && fire && !tReset
    awBuf.io.enq.valid := tNasti.aw.valid && fire && !tReset
    wBuf.io.enq.valid  := tNasti.w.valid  && fire && !tReset
    arBuf.io.enq.bits  := tNasti.ar.bits
    awBuf.io.enq.bits  := tNasti.aw.bits
    wBuf.io.enq.bits   := tNasti.w.bits

    // Response
    tNasti.r.bits     := rBuf.io.deq.bits
    tNasti.b.bits     := bBuf.io.deq.bits
    tNasti.r.valid    := rBuf.io.deq.valid && rCycleValid
    tNasti.b.valid    := bBuf.io.deq.valid && wCycleValid
    rBuf.io.deq.ready := tNasti.r.ready && fire && rCycleValid
    bBuf.io.deq.ready := tNasti.b.ready && fire && wCycleValid

    val cycles = Reg(UInt(width=64))
    cycles suggestName "cycles"
    when (fire) {
      cycles := Mux(targetReset, UInt(0), cycles + UInt(1))
    }

    (fire, cycles, targetReset)
  }
}

// Widget to handle NastiIO efficiently when mem models are not available
class NastiWidget(implicit val p: Parameters) extends NastiWidgetBase {
  val deltaBuf = Module(new Queue(UInt(width=32), 2))
  val delta = Reg(UInt(width=32))
  val readCount = Reg(UInt(width=32))
  val writeCount = Reg(UInt(width=32))
  val stall = !delta.orR && (readCount.orR || writeCount.orR)
  val (fire, cycles, targetReset) = elaborate(stall)

  deltaBuf.io.deq.ready := stall
  when(reset || targetReset) {
    delta := UInt(0)
  }.elsewhen(deltaBuf.io.deq.valid && stall) {
    delta := deltaBuf.io.deq.bits
  }.elsewhen(fire && delta.orR) {
    delta := delta - UInt(1)
  }

  when(reset || targetReset) {
    readCount := UInt(0)
  }.elsewhen(tNasti.ar.fire() && fire) {
    readCount := readCount + UInt(1)
  }.elsewhen(rBuf.io.enq.fire() && rBuf.io.enq.bits.last) {
    readCount := readCount - UInt(1)
  }

  when(reset || targetReset) {
    writeCount := UInt(0)
  }.elsewhen(tNasti.w.fire() && tNasti.w.bits.last && fire) {
    writeCount := writeCount + UInt(1)
  }.elsewhen(bBuf.io.enq.fire()) {
    writeCount := writeCount - UInt(1)
  }

  // Disable host_mem
  io.host_mem.ar.valid := Bool(false)
  io.host_mem.aw.valid := Bool(false)
  io.host_mem.w.valid := Bool(false)
  io.host_mem.r.ready := Bool(false)
  io.host_mem.b.ready := Bool(false)

  // Generate control register file
  val arMeta = Seq(arBuf.io.deq.bits.id, arBuf.io.deq.bits.size, arBuf.io.deq.bits.len)
  val arMetaWidth = (arMeta foldLeft 0)(_ + _.getWidth)
  assert(arMetaWidth <= io.ctrl.nastiXDataBits)
  if (tNasti.ar.bits.addr.getWidth + arMetaWidth <= io.ctrl.nastiXDataBits) {
    genROReg(Cat(arBuf.io.deq.bits.addr +: arMeta), "ar_bits")
  } else {
    genROReg(arBuf.io.deq.bits.addr, "ar_addr")
    genROReg(Cat(arMeta), "ar_meta")
  }

  val awMeta = Seq(awBuf.io.deq.bits.id, awBuf.io.deq.bits.size, awBuf.io.deq.bits.len)
  val awMetaWidth = (awMeta foldLeft 0)(_ + _.getWidth)
  assert(awMetaWidth <= io.ctrl.nastiXDataBits)
  if (tNasti.aw.bits.addr.getWidth + awMetaWidth <= io.ctrl.nastiXDataBits) {
    genROReg(Cat(awBuf.io.deq.bits.addr +: awMeta), "aw_bits")
  } else {
    genROReg(awBuf.io.deq.bits.addr, "aw_addr")
    genROReg(Cat(awMeta), "aw_meta")
  }

  val wMeta = Seq(wBuf.io.deq.bits.strb, wBuf.io.deq.bits.last)
  val wMetaWidth = (wMeta foldLeft 0)(_ + _.getWidth)
  assert(wMetaWidth <= io.ctrl.nastiXDataBits)
  genROReg(Cat(wMeta), "w_meta")
  val wdataChunks = (tNasti.w.bits.nastiXDataBits - 1) / io.ctrl.nastiXDataBits + 1
  val wdataRegs = Seq.fill(wdataChunks)(Reg(UInt()))
  val wdataAddrs = wdataRegs.zipWithIndex map {case (reg, i) =>
    reg := wBuf.io.deq.bits.data >> UInt(io.ctrl.nastiXDataBits*i)
    attach(reg, s"w_data_$i")
  }

  val rMeta = Seq(rBuf.io.deq.bits.id, rBuf.io.deq.bits.resp, rBuf.io.deq.bits.last)
  val rMetaWidth = (rMeta foldLeft 0)(_ + _.getWidth)
  val rMetaReg = Reg(UInt(width=rMetaWidth))
  assert(rMetaWidth <= io.ctrl.nastiXDataBits)
  attach(rMetaReg, "r_meta")
  rBuf.io.enq.bits.id := rMetaReg >> UInt(tNasti.r.bits.resp.getWidth + 1)
  rBuf.io.enq.bits.resp := rMetaReg >> UInt(1)
  rBuf.io.enq.bits.last := rMetaReg(0)
  val rdataChunks = (tNasti.r.bits.nastiXDataBits - 1) / io.ctrl.nastiXDataBits + 1
  val rdataRegs = Seq.fill(rdataChunks)(Reg(UInt()))
  val rdataAddrs = rdataRegs.zipWithIndex map {case (reg, i) => attach(reg, s"r_data_$i")}
  rBuf.io.enq.bits.data := Cat(rdataRegs.reverse)

  val bMeta = Seq(bBuf.io.deq.bits.id, bBuf.io.deq.bits.resp)
  val bMetaWidth = (bMeta foldLeft 0)(_ + _.getWidth)
  val bMetaReg = Reg(UInt(width=bMetaWidth))
  assert(bMetaWidth <= io.ctrl.nastiXDataBits)
  attach(bMetaReg, "b_meta")
  bBuf.io.enq.bits.id := bMetaReg >> UInt(tNasti.b.bits.resp.getWidth)
  bBuf.io.enq.bits.resp := bMetaReg

  genROReg(Cat(
    arBuf.io.deq.valid,
    awBuf.io.deq.valid,
    wBuf.io.deq.valid,
    rBuf.io.enq.ready,
    bBuf.io.enq.ready), "valid")
  val readyReg = RegInit(UInt(0, 5))
  arBuf.io.deq.ready := readyReg(4)
  awBuf.io.deq.ready := readyReg(3)
  wBuf.io.deq.ready := readyReg(2)
  rBuf.io.enq.valid := readyReg(1)
  bBuf.io.enq.valid := readyReg(0)
  attach(readyReg, "ready")
  when(readyReg.orR) { readyReg := UInt(0) }

  genROReg(!targetFire, "done")
  genROReg(stall, "stall")
  attachDecoupledSink(deltaBuf.io.enq, "delta")

  genCRFile()

  override def genHeader(base: BigInt, sb: StringBuilder) {
    super.genHeader(base, sb)
    import CppGenerationUtils._
    val name = getWName.toUpperCase
    sb.append(genArray(s"${name}_w_data", wdataAddrs map (off => UInt32(base + off)))) 
    sb.append(genArray(s"${name}_r_data", rdataAddrs map (off => UInt32(base + off)))) 
  }
}