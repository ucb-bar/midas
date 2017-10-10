package midas
package widgets

import midas.core.{NumAsserts, PrintPorts, PrintRecord}
import chisel3._
import chisel3.util._
import config.Parameters

class AssertWidgetIO(implicit p: Parameters) extends WidgetIO()(p) {
  val tReset = Flipped(Decoupled(Bool()))
  val assert = Flipped(Decoupled(UInt((log2Ceil(p(NumAsserts) max 1) + 1).W)))
}

class AssertWidget(implicit p: Parameters) extends Widget()(p) with HasChannels {
  val io = IO(new AssertWidgetIO)
  val resume = Wire(init=false.B)
  val cycles = Reg(UInt(64.W))
  val assertId = io.assert.bits >> 1
  val assertFire = io.assert.bits(0) && !io.tReset.bits && cycles.orR
  val fire = io.assert.valid && io.tReset.valid && (!assertFire || resume)
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
  Pulsify(genWORegInit(resume, "resume", false.B), pulseLength = 1)
  genCRFile()
}

class PrintWidgetIO(implicit p: Parameters) extends WidgetIO()(p) {
  val tReset = Flipped(Decoupled(Bool()))
  val prints = Flipped(Decoupled(new PrintRecord(p(PrintPorts))))
}

class PrintWidget(implicit p: Parameters) extends Widget()(p) with HasChannels {
  val io = IO(new PrintWidgetIO)
  val fire = Wire(Bool())
  val cycles = Reg(UInt(64.W))
  val enable = RegInit(false.B)
  val enableAddr = attach(enable, "enable")
  val bitsAddrs = collection.mutable.ArrayBuffer[Int]()
  val bitsChunks = collection.mutable.ArrayBuffer[Int]()
  val deltaAddrs = collection.mutable.ArrayBuffer[Int]()
  val countAddrs = collection.mutable.ArrayBuffer[Int]()
  val prints = io.prints.bits.elements.zipWithIndex map { case ((_, elem), i) =>
    val width = elem.getWidth - 1
    val addrs = collection.mutable.ArrayBuffer[Int]()
    val printFire = fire && enable && elem(0) && !io.tReset.bits
    val ready = (0 until width by io.ctrl.nastiXDataBits).zipWithIndex map { case (off, j) =>
      val queue = Module(new Queue(UInt(io.ctrl.nastiXDataBits.W), p(strober.core.TraceMaxLen)))
      queue.io.enq.bits := (elem >> 1.U) >> off.U
      queue.io.enq.valid := printFire
      addrs += attachDecoupledSource(queue.io.deq, s"prints_${i}_bits_${j}")
      when(false.B) { printf("%d", queue.io.count) }
      queue suggestName s"queue_${i}_${j}"
      queue.io.enq.ready
    }
    bitsAddrs  += addrs.head
    bitsChunks += addrs.size
    // FIXME: too big?
    val delta = Module(new Queue(UInt(32.W), p(strober.core.TraceMaxLen)))
    val last  = RegEnable(Mux(io.tReset.bits, 0.U, cycles), fire && enable && (elem(0) || io.tReset.bits))
    delta suggestName s"delta_${i}"
    delta.io.enq.bits  := cycles - last
    delta.io.enq.valid := printFire
    deltaAddrs += attachDecoupledSource(delta.io.deq, s"prints_${i}_delta")
    countAddrs += attach(RegNext(delta.io.count), s"prints_${i}_count", ReadOnly)
    (ready foldLeft delta.io.enq.ready)(_ && _)
  }
  fire := (prints foldLeft (io.prints.valid && io.tReset.valid))(_ && _)
  io.tReset.ready := fire
  io.prints.ready := fire
  when (fire) {
    cycles := Mux(io.tReset.bits, 0.U, cycles + 1.U)
  }

  override def genHeader(base: BigInt, sb: StringBuilder) {
    import CppGenerationUtils._
    sb.append(genComment("Print Widget"))
    sb.append(genMacro("PRINTS_NUM", UInt32(p(PrintPorts).size)))
    sb.append(genMacro("PRINTS_ENABLE", UInt32(base + enableAddr)))
    sb.append(genArray("PRINTS_BITS_ADDRS", bitsAddrs.toSeq map (x => UInt32(base + x))))
    sb.append(genArray("PRINTS_BITS_CHUNKS", bitsChunks.toSeq map (UInt32(_))))
    sb.append(genArray("PRINTS_DELTA_ADDRS", deltaAddrs.toSeq map (x => UInt32(base + x))))
    sb.append(genArray("PRINTS_COUNT_ADDRS", countAddrs.toSeq map (x => UInt32(base + x))))
  }

  genCRFile()
}
