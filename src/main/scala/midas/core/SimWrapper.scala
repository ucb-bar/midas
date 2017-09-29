package midas
package core

import scala.collection.immutable.ListMap
import scala.collection.mutable.{ArrayBuffer, HashSet}

// from rocketchip
import junctions.NastiIO
import freechips.rocketchip.amba.axi4.AXI4Bundle
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.util.HeterogeneousBag

import chisel3._
import chisel3.internal.firrtl.Port
import chisel3.util._
import chisel3.experimental._
import SimUtils._

object SimUtils {
  def parsePorts(io: Data, reset: Option[Bool] = None, prefix: String = "") = {
    val inputs = ArrayBuffer[(Bits, String)]()
    val outputs = ArrayBuffer[(Bits, String)]()

    def prefixWith(prefix: String, base: Any): String  = if (prefix != "")  s"${prefix}_${base}" else base.toString

    def loop(name: String, data: Data): Unit = data match {
      case b: Record =>
        b.elements foreach {case (n, e) => loop(prefixWith(name, n), e)}
      case v: Vec[_] =>
        v.zipWithIndex foreach {case (e, i) => loop(prefixWith(name, i), e)}
      case b: Bits if b.dir == INPUT => inputs += (b -> name)
      case b: Bits if b.dir == OUTPUT => outputs += (b -> name)
      case b: Clock if b.dir == INPUT => Nil
      //case b: Clock if b.dir == OUTPUT => outputs += (b.asUInt -> name)
      case default =>
        default.asInstanceOf[HeterogeneousBag[AXI4Bundle]].elements foreach {case (n, e) => loop(prefixWith(name, n), e)}
    }

    loop(prefix, io)
    (inputs.toList, outputs.toList)
  }

  def getChunks(b: Data)(implicit channelWidth: Int): Int =
    (b.getWidth-1)/channelWidth + 1
  def getChunks(s: Seq[Data])(implicit channelWidth: Int): Int =
    (s foldLeft 0)((res, b) => res + getChunks(b))
  def getChunks(args: (Data, String))(implicit channelWidth: Int): (String, Int) =
    args match { case (wire, name) => name -> SimUtils.getChunks(wire) }

  def genIoMap(ports: Seq[(Data, String)], offset: Int = 0)(implicit channelWidth: Int) =
    ((ports foldLeft ((ListMap[Data, Int](), offset))){
      case ((map, off), (port, name)) => (map + (port -> off), off + getChunks(port))
    })._1
}

case object ChannelLen extends Field[Int]
case object ChannelWidth extends Field[Int]

trait HasSimWrapperParams {
  implicit val p: Parameters
  implicit val channelWidth = p(ChannelWidth)
  val traceMaxLen = p(strober.core.TraceMaxLen)
  val daisyWidth = p(strober.core.DaisyWidth)
  val sramChainNum = p(strober.core.SRAMChainNum)
  val enableSnapshot = p(EnableSnapshot)
}

class SimReadyValidRecord(es: Seq[(String, ReadyValidIO[Data])]) extends Record {
  val elements = ListMap((es map {
    case (name, rv) if rv.valid.dir == INPUT => name -> Flipped(SimReadyValid(rv.bits))
    case (name, rv) if rv.valid.dir == OUTPUT => name -> SimReadyValid(rv.bits)
  }):_*)
  def cloneType = new SimReadyValidRecord(es).asInstanceOf[this.type]
}

class ReadyValidTraceRecord(es: Seq[(String, ReadyValidIO[Data])]) extends Record {
  val elements = ListMap((es map {
    case (name, rv) => name -> ReadyValidTrace(rv.bits)
  }):_*)
  def cloneType = new ReadyValidTraceRecord(es).asInstanceOf[this.type]
}

class SimWrapperIO(
    io: Record, reset: Bool)
   (implicit val p: Parameters) extends Bundle with HasSimWrapperParams {
  /*** Endpoints ***/
  val endpointMap = p(EndpointKey)
  val endpoints = endpointMap.endpoints
  private def findEndpoint(name: String, data: Data) {
    endpointMap get data match {
      case Some(endpoint) =>
        endpoint add (name, data)
      case None => data match {
        case b: Record => b.elements foreach {
          case (n, e) => findEndpoint(s"${name}_${n}", e)
        }
        case v: Vec[_] => v.zipWithIndex foreach {
          case (e, i) => findEndpoint(s"${name}_${i}", e)
        }
        case h: HeterogeneousBag[AXI4Bundle] => h.elements foreach {
          case (n, e) => findEndpoint(s"${name}_${n}", e)
        }
        case _ =>
      }
    }
  }
  io.elements.foreach({ case (name, data) => findEndpoint(name, data)})

  val (inputs, outputs) = parsePorts(io, Some(reset))

  /*** Wire Channels ***/
  val endpointWires = (endpoints flatMap (ep => (0 until ep.size) flatMap { i =>
    val (prefix, data) = ep(i)
    data.elements.toSeq flatMap {
      case (name, rv: ReadyValidIO[_]) => Nil
      case (name, wires) =>
        val (ins, outs) = parsePorts(wires, prefix = name)
        (ins ++ outs).unzip._1
    }
  })).toSet
  val wireInputs = inputs filterNot { case (wire, name) =>
    (endpoints exists (_(wire))) && !endpointWires(wire) }
  val wireOutputs = outputs filterNot { case (wire, name) =>
    (endpoints exists (_(wire))) && !endpointWires(wire) }
  val pokedInputs = wireInputs filterNot (x => endpointWires(x._1))
  val peekedOutputs = wireOutputs filterNot (x => endpointWires(x._1))
  val inWireChannelNum = getChunks(wireInputs.unzip._1)
  val outWireChannelNum = getChunks(wireOutputs.unzip._1)
  val wireIns = Flipped(Vec(inWireChannelNum, Decoupled(UInt(channelWidth.W))))
  val wireOuts = Vec(outWireChannelNum, Decoupled(UInt(channelWidth.W)))
  val wireInMap = genIoMap(wireInputs)
  val wireOutMap = genIoMap(wireOutputs)
  def getIns(arg: (Data, Int)): Seq[DecoupledIO[UInt]] = arg match {
    case (wire, id) => (0 until getChunks(wire)) map (off => wireIns(id+off))
  }
  def getOuts(arg: (Data, Int)): Seq[DecoupledIO[UInt]] = arg match {
    case (wire, id) => (0 until getChunks(wire)) map (off => wireOuts(id+off))
  }
  def getIns(wire: Data): Seq[DecoupledIO[UInt]] = getIns(wire -> wireInMap(wire))
  def getOuts(wire: Data): Seq[DecoupledIO[UInt]] = getOuts(wire -> wireOutMap(wire))

  /*** ReadyValid Channels ***/
  val readyValidInputs = endpoints flatMap (ep => (0 until ep.size) flatMap { i =>
    val (prefix, data) = ep(i)
    data.elements.toSeq collect {
      case (name, rv: ReadyValidIO[_]) if rv.valid.dir == INPUT =>
        s"${prefix}_${name}" -> rv
    }
  })
  val readyValidOutputs = endpoints flatMap (ep => (0 until ep.size) flatMap { i =>
    val (prefix, data) = ep(i)
    data.elements.toSeq collect {
      case (name, rv: ReadyValidIO[_]) if rv.valid.dir == OUTPUT =>
        s"${prefix}_${name}" -> rv
    }
  })
  val readyValidIns = new SimReadyValidRecord(readyValidInputs)
  val readyValidOuts = new SimReadyValidRecord(readyValidOutputs)
  val readyValidInMap = (readyValidInputs.unzip._2 zip readyValidIns.elements).toMap
  val readyValidOutMap = (readyValidOutputs.unzip._2 zip readyValidOuts.elements).toMap
  val readyValidMap = readyValidInMap ++ readyValidOutMap

  /*** Instrumentation ***/
  val daisy = new strober.core.DaisyBundle(daisyWidth, sramChainNum)
  val traceLen = Input(UInt(log2Up(traceMaxLen + 1).W))
  val wireInTraces = Vec(if (enableSnapshot) inWireChannelNum else 0, Decoupled(UInt(channelWidth.W)))
  val wireOutTraces = Vec(if (enableSnapshot) outWireChannelNum else 0, Decoupled(UInt(channelWidth.W)))
  val readyValidInTraces = new ReadyValidTraceRecord(if (enableSnapshot) readyValidInputs else Nil)
  val readyValidOutTraces = new ReadyValidTraceRecord(if (enableSnapshot) readyValidOutputs else Nil)

  override def cloneType: this.type =
    new SimWrapperIO(io, reset).asInstanceOf[this.type]
}

class RecordWithClockAndReset(targetIo: Record) extends Record {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val elements = ListMap(((for ((field, elm) <- targetIo.elements) yield {
      (field -> elm.chiselCloneType)
    }).toSeq ++ Seq("clock" -> clock, "reset" -> reset)):_*)

  override def cloneType = new RecordWithClockAndReset(targetIo).asInstanceOf[this.type]
}

// this gets replaced with the real target
class TargetBox(targetIo: Record) extends BlackBox {
  val io = IO(new RecordWithClockAndReset(targetIo))
}

class SimBox(simIo: SimWrapperIO)
            (implicit val p: Parameters)
             extends BlackBox with HasSimWrapperParams {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val io = simIo.cloneType
  })
}

class SimWrapper(targetIo: Record)
                (implicit val p: Parameters) extends Module with HasSimWrapperParams {
  val target = Module(new TargetBox(targetIo))
  val io = IO(new SimWrapperIO(target.io, target.io.reset))
  val fire = Wire(Bool())

  target.io.clock := clock

  /*** Wire Channels ***/
  val wireInChannels: Seq[WireChannel] = io.wireInputs flatMap genWireChannels
  val wireOutChannels: Seq[WireChannel] = io.wireOutputs flatMap genWireChannels

  (wireInChannels zip io.wireIns) foreach { case (channel, in) => channel.io.in <> in }
  (io.wireInputs foldLeft 0)(connectInput(_, _, wireInChannels, fire))

  (io.wireOuts zip wireOutChannels) foreach { case (out, channel) => out <> channel.io.out }
  (io.wireOutputs foldLeft 0)(connectOutput(_, _, wireOutChannels))

  if (enableSnapshot) {
    (io.wireInTraces zip wireInChannels) foreach { case (tr, channel) => tr <> channel.io.trace }
    (io.wireOutTraces zip wireOutChannels) foreach { case (tr, channel) => tr <> channel.io.trace }
  }

  def genWireChannels[T <: Data](arg: (T, String)) =
    arg match { case (port, name) =>
      (0 until getChunks(port)) map { off =>
        val width = scala.math.min(channelWidth, port.getWidth - off * channelWidth)
        val channel = Module(new WireChannel(width))
        channel suggestName s"WireChannel_${name}_${off}"
        channel
      }
    }

  def connectInput[T <: Data](off: Int, arg: (Data, String), inChannels: Seq[WireChannel], fire: Bool) =
    arg match { case (wire, name) =>
      val channels = inChannels slice (off, off + getChunks(wire))
      wire match {
        case b :Bits => wire := Cat(channels.reverse map (_.io.out.bits))
        case b :Clock => wire := Cat(channels.reverse map (_.io.out.bits))(0).asClock
      }
      off + getChunks(wire)
    }

  def connectOutput[T <: Data](off: Int, arg: (Data, String), outChannels: Seq[WireChannel]) =
    arg match { case (wire, name) =>
      val channels = outChannels slice (off, off + getChunks(wire))
      channels.zipWithIndex foreach { case (channel, i) =>
        channel.io.in.bits := wire.asUInt() >> (i * channelWidth)
      }
      off + getChunks(wire)
    }

  /*** ReadyValid Channels ***/
  val readyValidInChannels: Seq[ReadyValidChannel[_]] = io.readyValidInputs map genReadyValidChannel
  val readyValidOutChannels: Seq[ReadyValidChannel[_]] = io.readyValidOutputs map genReadyValidChannel

  (readyValidInChannels zip io.readyValidIns.elements.unzip._2) foreach {
    case (channel, in) => channel.io.enq <> in }
  (io.readyValidOuts.elements.unzip._2 zip readyValidOutChannels) foreach {
    case (out, channel) => out <> channel.io.deq }

  if (enableSnapshot) {
    (io.readyValidInTraces.elements.unzip._2 zip readyValidInChannels) foreach {
      case (tr, channel) => tr <> channel.io.trace }
    (io.readyValidOutTraces.elements.unzip._2 zip readyValidOutChannels) foreach {
      case (tr, channel) => tr <> channel.io.trace }
  }

  def genReadyValidChannel[T <: Data](arg: (String, ReadyValidIO[T])) =
    arg match { case (name, io) =>
      val channel = Module(new ReadyValidChannel(io.bits, io.valid.dir == INPUT))
      channel suggestName s"ReadyValidChannel_$name"
      (io.valid.dir: @unchecked) match {
        case INPUT  => io <> channel.io.deq.target
        case OUTPUT => channel.io.enq.target <> io
      }
      channel.io.targetReset.bits := target.io.reset
      channel.io.targetReset.valid := fire
      channel
    }

  // Control
  // Firing condtion:
  // 1) all input values are valid
  // 2) all output FIFOs are not full
  fire := (wireInChannels foldLeft true.B)(_ && _.io.out.valid) && 
          (wireOutChannels foldLeft true.B)(_ && _.io.in.ready) &&
          (readyValidInChannels foldLeft true.B)(_ && _.io.deq.host.hValid) &&
          (readyValidOutChannels foldLeft true.B)(_ && _.io.enq.host.hReady)
 
  // Inputs are consumed when firing conditions are met
  wireInChannels foreach (_.io.out.ready := fire)
  readyValidInChannels foreach (_.io.deq.host.hReady := fire)
   
  // Outputs should be ready when firing conditions are met
  val resetNext = RegNext(reset)
  wireOutChannels foreach (_.io.in.valid := fire || resetNext)
  readyValidOutChannels foreach (_.io.enq.host.hValid := fire || resetNext)

  // Trace size is runtime configurable
  wireInChannels foreach (_.io.traceLen := io.traceLen)
  wireOutChannels foreach (_.io.traceLen := io.traceLen)
  readyValidInChannels foreach (_.io.traceLen := io.traceLen)
  readyValidOutChannels foreach (_.io.traceLen := io.traceLen)

  // Cycles for debug
  val cycles = Reg(UInt(64.W))
  when (fire) {
    cycles := Mux(target.io.reset, UInt(0), cycles + UInt(1))
    when(false.B) { printf("%d", cycles) }
  }
} 
