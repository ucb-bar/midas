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
  val validAddrs = collection.mutable.ArrayBuffer[Int]()
  val readyAddrs = collection.mutable.ArrayBuffer[Int]()
  val bitsAddrs = collection.mutable.ArrayBuffer[Int]()
  val bitsChunks = collection.mutable.ArrayBuffer[Int]()
  val deltaAddrs = collection.mutable.ArrayBuffer[Int]()
  val prints = io.prints.bits.elements.zipWithIndex map { case ((_, elem), i) =>
    val width = elem.getWidth - 1
    val queue = Module(new Queue(UInt(width.W), p(strober.core.TraceMaxLen), flow=true))
    // FIXME: too big?
    val delta = Module(new Queue(UInt(32.W),    p(strober.core.TraceMaxLen), flow=true))
    val last  = RegEnable(Mux(io.tReset.bits, 0.U, cycles),
                          queue.io.enq.valid || fire && io.tReset.bits)
    val readyReg = RegInit(false.B)
    val validReg = RegNext(queue.io.deq.valid)
    queue suggestName s"queue_${i}"
    delta suggestName s"delta_${i}"
    when(false.B) { printf("%d", queue.io.count) }
    when(false.B) { printf("%d", delta.io.count) }
    queue.io.enq.bits  := elem >> 1.U
    queue.io.enq.valid := elem(0) && fire && !io.tReset.bits && enable
    delta.io.enq.bits  := cycles - last
    delta.io.enq.valid := queue.io.enq.valid
    readyAddrs += attach(readyReg, s"prints_${i}_ready", WriteOnly)
    validAddrs += attach(validReg, s"prints_${i}_valid", ReadOnly)
    val bits = (0 until width by io.ctrl.nastiXDataBits).zipWithIndex map { case (low, j) =>
      val high = ((low + io.ctrl.nastiXDataBits) min width) - 1
      val reg = RegNext(queue.io.deq.bits(high, low))
      attach(reg, s"prints_${i}_bits_${j}", ReadOnly)
    }
    bitsAddrs += bits.head
    bitsChunks += bits.size
    deltaAddrs += attachDecoupledSource(delta.io.deq, s"prints_${i}_delta")
    queue.io.deq.ready := readyReg
    Pulsify(readyReg, pulseLength = 1)
    queue.io.enq.ready && delta.io.enq.ready
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
    sb.append(genArray("PRINTS_READY_ADDRS", readyAddrs.toSeq map (x => UInt32(base + x))))
    sb.append(genArray("PRINTS_VALID_ADDRS", validAddrs.toSeq map (x => UInt32(base + x))))
    sb.append(genArray("PRINTS_BITS_ADDRS", bitsAddrs.toSeq map (x => UInt32(base + x))))
    sb.append(genArray("PRINTS_BITS_CHUNKS", bitsChunks.toSeq map (UInt32(_))))
    sb.append(genArray("PRINTS_DELTA_ADDRS", deltaAddrs.toSeq map (x => UInt32(base + x))))
  }

  genCRFile()
}
