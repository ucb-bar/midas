package strober
package testers

import junctions._
import midas_widgets._
import scala.collection.mutable.{HashMap, ArrayBuffer, Queue => ScalaQueue}
import java.io.{File, InputStream}

private case class NastiReadAddr(id: Int, addr: Int, size: Int = 0, len: Int = 0)
private case class NastiReadData(id: Int, data: BigInt, last: Boolean = true)
private case class NastiWriteAddr(id: Int, addr: Int, size: Int = 0, len: Int = 0)
private case class NastiWriteData(data: BigInt, last: Boolean = true)
private case class NastiWriteResp(id: Int, resp: Int = 0)
 
trait LoadMemType
case object FastLoadMem extends LoadMemType
case object SlowLoadMem extends LoadMemType

abstract class ZynqShimTester[+T <: SimNetwork](
    c: ZynqShim[T],
    verbose: Boolean = true,
    sampleFile: Option[File] = None,
    logFile: Option[File] = None,
    loadmemType: LoadMemType = SlowLoadMem) extends StroberTester(c, verbose, sampleFile, logFile) {
  private implicit def bigIntToInt(b: BigInt) = b.toInt

  private val MAXI_aw = new ChannelSource(c.io.master.aw, (aw: NastiWriteAddressChannel, in: NastiWriteAddr) =>
    { _poke(aw.id, in.id) ; _poke(aw.addr, in.addr) })
  private val MAXI_w = new ChannelSource(c.io.master.w, (w: NastiWriteDataChannel, in: NastiWriteData) =>
    { _poke(w.data, in.data) ; _poke(w.last, in.last) ; _poke(w.strb, (BigInt(1) << w.nastiWStrobeBits) - 1) })
  private val MAXI_b = new ChannelSink(c.io.master.b, (b: NastiWriteResponseChannel) =>
    new NastiWriteResp(_peek(b.id), _peek(b.resp)), alwaysReady = false)
  private val MAXI_ar = new ChannelSource(c.io.master.ar, (ar: NastiReadAddressChannel, in: NastiReadAddr) =>
    { _poke(ar.id, in.id) ; _poke(ar.addr, in.addr) })
  private val MAXI_r = new ChannelSink(c.io.master.r, (r: NastiReadDataChannel) =>
    new NastiReadData(_peek(r.id), _peek(r.data), _peek(r.last)), alwaysReady = false)
 
  private val addrOffset = chisel3.util.log2Up(c.master.nastiXAddrBits/8)

  protected[testers] def pokeChannel(addr: Int, data: BigInt) {
    MAXI_aw.inputs enqueue (new NastiWriteAddr(0, addr << addrOffset))
    MAXI_w.inputs enqueue (new NastiWriteData(data))
    MAXI_b.allocate
    Predef.assert(_eventually(MAXI_b.noPendingResps), "no poke response")
    MAXI_b.outputs.clear
  }

  protected[testers] def peekChannel(addr: Int) = {
    MAXI_ar.inputs enqueue (new NastiReadAddr(0, addr << addrOffset))
    MAXI_r.allocate
    Predef.assert(_eventually(MAXI_r.noPendingResps), "no peek value")
    MAXI_r.outputs.dequeue.data
  }

  override def setTraceLen(len: Int) { 
    super.setTraceLen(len)
    writeCR("EmulationMaster", "TRACELEN", len)
  }

  def writeCR(w: Widget, crName: String, value: BigInt){
    val addr = c.getCRAddr(w, crName)
    pokeChannel(addr, value)
  }

  def writeCR(w: String, crName: String, value: BigInt){
    val addr = c.getCRAddr(w, crName)
    pokeChannel(addr, value)
  }

  def readCR(w: Widget, crName: String) = {
    val addr = c.getCRAddr(w, crName)
    peekChannel(addr)
  }

  def readCR(w: String, crName: String) = {
    val addr = c.getCRAddr(w, crName)
    peekChannel(addr)
  }
  override def reset(n: Int) {
    for (_ <- 0 until n) {
      writeCR("EmulationMaster", "HOST_RESET", 1)
      writeCR("EmulationMaster", "SIM_RESET", 1)
      Predef.assert(_eventually(readCR("EmulationMaster", "DONE") == BigInt(1)),
             "simulation is not done in time")
      _peekMap.clear
      // flush junk output tokens
      c.OUT_ADDRS foreach {case (out, addr) =>
        _peekMap(out) = peekChunks(addr, SimUtils.getChunks(out))
      }
      // flush junk traces
      super.reset(1)
    }
  }

  override def _tick(n: Int) {
    writeCR("EmulationMaster", "STEP", n)
    c.IN_ADDRS foreach {case (in, addr) =>
      pokeChunks(addr, SimUtils.getChunks(in), _pokeMap getOrElse (in, BigInt(rnd.nextInt)))
    }
    Predef.assert(_eventually(readCR("EmulationMaster", "DONE") == BigInt(1)),
           "simulation is not done in time")
    c.OUT_ADDRS foreach {case (out, addr) =>
      _peekMap(out) = peekChunks(addr, SimUtils.getChunks(out))
    }
  }

  private val SAXI_aw = new ChannelSink(c.io.slave.aw, (aw: NastiWriteAddressChannel) =>
    new NastiWriteAddr(_peek(aw.id), _peek(aw.addr), _peek(aw.size), _peek(aw.len)))
  private val SAXI_w = new ChannelSink(c.io.slave.w, (w: NastiWriteDataChannel) =>
    new NastiWriteData(_peek(w.data), _peek(w.last)))
  private val SAXI_b = new ChannelSource(c.io.slave.b, (b: NastiWriteResponseChannel, in: NastiWriteResp) =>
    { _poke(b.id, in.id) ; _poke(b.resp, in.resp) })
  private val SAXI_ar = new ChannelSink(c.io.slave.ar, (ar: NastiReadAddressChannel) =>
    new NastiReadAddr(_peek(ar.id), _peek(ar.addr), _peek(ar.size), _peek(ar.len)))
  private val SAXI_r = new ChannelSource(c.io.slave.r, (r: NastiReadDataChannel, in: NastiReadData) =>
    { _poke(r.id, in.id) ; _poke(r.data, in.data) ; _poke(r.last, in.last) })
 
  private def parseNibble(hex: Int) = if (hex >= 'a') hex - 'a' + 10 else hex - '0'

  private class NastiMem(
      arQ: ScalaQueue[NastiReadAddr],  rQ: ScalaQueue[NastiReadData],
      awQ: ScalaQueue[NastiWriteAddr], wQ: ScalaQueue[NastiWriteData],
      bQ: ScalaQueue[NastiWriteResp], latency: Int = 5, word_width: Int = 4,
      depth: Int = 1 << 20) extends chisel3.iotesters.Processable {
    import chisel3.util.log2Up
    require(word_width % 4 == 0, "word_width should be divisible by 4")
    private val addrMask = (1 << log2Up(depth))-1
    private val mem = Array.fill(depth){BigInt(0)}
    private def int(b: Byte) = (BigInt((b >>> 1) & 0x7f) << 1) | b & 0x1
    private val off = log2Up(word_width)
    private def read(addr: Int) = {
      if (addr > (1 << 21)) println(s"read addr: $addr")
      val data = mem(addr & addrMask)
      logger println "MEM[%x] => %x".format(addr & addrMask, data)
      data
    }
    private def write(addr: Int, data: BigInt) {
      logger println "MEM[%x] <= %x".format(addr & addrMask, data)
      mem(addr & addrMask) = data
    }
    private def loadMem(lines: Iterator[String]) {
      for ((line, i) <- lines.zipWithIndex) {
        val base = (i * line.length) / 2
        Predef.assert(base % word_width == 0)
        ((0 until line.length by 2) foldRight (BigInt(0), 0)){case (k, (data, offset)) =>
          val shift = 8 * (offset % word_width)
          val byte = ((parseNibble(line(k)) << 4) | parseNibble(line(k+1))).toByte
          if ((offset % word_width) == word_width - 1) {
            mem((base+offset)>>off) = data | int(byte) << shift
            (BigInt(0), offset + 1)
          } else {
            (data | int(byte) << shift, offset + 1)
          }
        }
      }
    }
    def loadMem(file: File) {
      loadMem(io.Source.fromFile(file).getLines)
    }
    def loadMem(stream: InputStream) {
      loadMem(io.Source.fromInputStream(stream).getLines)
    }

    private var aw: Option[NastiWriteAddr] = None
    private val schedule = Array.fill(latency){ScalaQueue[NastiReadData]()}
    private var cur_cycle = 0
    def process {
      aw match {
        case Some(p) if wQ.size > p.len =>
          Predef.assert((1 << p.size) == word_width)
          (0 to p.len) foreach (i =>
            write((p.addr >> off) + i, wQ.dequeue.data))
          bQ enqueue new NastiWriteResp(p.id)
          aw = None
        case None if !awQ.isEmpty => aw = Some(awQ.dequeue)
        case None if !arQ.isEmpty =>
          val ar = arQ.dequeue
          (0 to ar.len) foreach (i =>
            schedule((cur_cycle+latency-1) % latency) enqueue
              new NastiReadData(ar.id, read((ar.addr >> off) + i), i == ar.len))
        case _ =>
      }
      while (!schedule(cur_cycle).isEmpty) {
        rQ enqueue schedule(cur_cycle).dequeue
      }
      cur_cycle = (cur_cycle + 1) % latency
    }

    _preprocessors += this
  }
 
  private val mem = new NastiMem(
    SAXI_ar.outputs, SAXI_r.inputs, SAXI_aw.outputs, SAXI_w.outputs, SAXI_b.inputs,
    word_width=c.arb.nastiXDataBits/8)

  def loadMem(file: File) = loadmemType match {
    case FastLoadMem => mem loadMem file
    case SlowLoadMem => slowLoadMem(file)
  }

  def loadMem(stream: InputStream) = loadmemType match {
    case FastLoadMem => mem loadMem stream
    case SlowLoadMem => slowLoadMem(stream)
  }

  def writeMem(addr: BigInt, data: BigInt) {
    val w = c.widgets.flatMap {
      case x: LoadMemWidget => Some(x)
      case _ => None
    } head

    writeCR(w, "W_ADDRESS", addr)
    //Loadmem unit expects MSWs first
    ((w.widthRatio - 1) to 0 by -1) foreach { i =>
      writeCR(w, s"W_DATA", data >> BigInt(i*w.cWidth))
    }
  }

  def readMem(addr: BigInt): BigInt = {
    val w = c.widgets flatMap{case (x: LoadMemWidget) => Some(x)} head

    writeCR(w, "R_ADDRESS", addr)
    (0 until w.widthRatio).foldLeft(BigInt(0))((data, idx) =>
      data + (readCR(w, "R_DATA") << (idx * w.cWidth)))
  }

  private def slowLoadMem(lines: Iterator[String]) {
    val chunk = c.arb.nastiXDataBits / 4
    lines.zipWithIndex foreach {case (line, i) =>
      val base = (i * line.length) / 2
      Predef.assert(line.length % chunk == 0)
      (((line.length - chunk) to 0 by -chunk) foldLeft 0){ (offset, j) =>
        writeMem(base+offset, ((0 until chunk) foldLeft BigInt(0)){ (res, k) =>
          res | (BigInt(parseNibble(line(j+k))) << (4*(chunk-1-k)))
        })
        offset + chunk / 2
      }
    }

  }

  def slowLoadMem(file: File) {
    println(s"[LOADMEM] LOADING $file")
    slowLoadMem(scala.io.Source.fromFile(file).getLines)
    println(s"[LOADMEM] DONE")
  }

  def slowLoadMem(stream: InputStream) {
    slowLoadMem(scala.io.Source.fromInputStream(stream).getLines)
  }

  protected[testers] def readChain(t: ChainType.Value) = {
    val controller = c.widgets flatMap{case (x: DaisyController) => Some(x)} head


    val chain = new StringBuilder
    for (_ <- 0 until chainLoop(t) ; i <- 0 until controller.io.daisy(t).size) {
      t match {
        case ChainType.SRAM => writeCR("DaisyChainController", s"SRAM_RESTART_$i", 1)
        case _ =>
      }
      for (_ <- 0 until chainLen(t)) {
        try {
          chain append intToBin(readCR("DaisyChainController", s"${t.toString.toUpperCase}_$i"), c.sim.daisyWidth)
        } catch {
          case e: java.lang.AssertionError =>
            assert(false, s"$t chain not available")
        }
      }
    }
    chain.result
  }

  reset(5)
}
