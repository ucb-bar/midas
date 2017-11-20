// See LICENSE for license details.

package midas
package endpoints

import core._
import widgets._

import config.Parameters

import chisel3._
import chisel3.util._

import scala.collection.immutable.ListMap

import scala.language.dynamics // for transparent wrapping of Bundle

/**
 * InstrumentationIO is a bundle-parametrized widget which collects output
 * information from a target design and enqueues into a queue for instrumentation
 * on the other side.
 *
 * It only supports Outputs() for now; future work will add support for
 * ValidIO or inputs.
 */

class InstrumentationIO(bundle: Bundle) extends Record with Dynamic {
  if (bundle.elements.isEmpty) {
    throw new IllegalArgumentException(s"Cannot use a bundle (${bundle}) with no elements. Note: if you are using an anonymous Bundle, create a named bundle and used that instead. (See chisel3 #721)")
  }

  override def toString: String = "InstrumentationIO(" + s"${bundle}" + ")"

  // For transparent access to bundle members.
  // e.g. if bundle had a member called foo, we can access it with instr_io.foo
  // instead of instr_io("foo").
  def selectDynamic(elt: String): Data = if (elt == "bundle") bundle else elements(elt) // we have to special case "bundle" otherwise it will try elements("bundle")

  // We don't support non-Outputs yet.
  // TODO: won't work until chisel3 #617.
  //~ bundle.elements.toSeq.map { case (field, elt) => require(elt.direction == Output, "All memebers of InstrumentationIO bundle must be Outputs") }

  // Basically clone the given bundle.
  val elements = ListMap(bundle.elements.toSeq.map { case (field, elt) => (field -> elt.chiselCloneType) }: _*)
  def apply(elt: String): Data = elements(elt)

  override def cloneType = new InstrumentationIO(bundle).asInstanceOf[this.type]
}

class SimInstrumentationIO extends Endpoint {
  def matchType(data: Data) = data match {
    case channel: InstrumentationIO => true
    case _ => false
  }
  def widget(channel: Option[Data] = None)(p: Parameters) = channel match {
    case Some(io: InstrumentationIO) => new InstrumentationWidget(io.bundle.asInstanceOf[Bundle])(p)
    case None => throw new IllegalArgumentException("InstrumentationIO widget requires the channel type!")
    case x => throw new IllegalArgumentException(s"Unknown channel type ${x}")
  }
  override def widgetName = "InstrumentationWidget"
}

class InstrumentationWidgetIO(bundle: Bundle)(implicit p: Parameters) extends EndpointWidgetIO()(p) {
  val hPort = Flipped(HostPort(new InstrumentationIO(bundle)))
}

class InstrumentationWidget(bundle: Bundle, queueDepth: Int = 8)(implicit p: Parameters) extends EndpointWidget()(p) {
  val io = IO(new InstrumentationWidgetIO(bundle))

  // Create queues to collect the data from the target.
  val outBufs: Map[String, (Queue[Data], Bool)] = Map(bundle.elements.toSeq.map { case (fieldName, element) => {
      val module = Module(new Queue(element, queueDepth))
      val blockingQueue = Wire(Bool())
      (fieldName, (module, blockingQueue))
  }} : _*)

  // Signal which tells us if at least one queue is full and blocking, in which case we need
  // to stop consuming tokens.
  val queueFull = outBufs.values.foldLeft(false.B)((signal, x) => {
    val (queue, blocking) = x
    signal || (!queue.io.enq.ready && blocking)
  })

  val target = io.hPort.hBits

  // Enqueue firing condition.
  // 1. Tokens from the target are presented (io.hPort.toHost.hValid)
  // 2. Queues aren't full (!queueFull)
  // 3. target reset tokens are presented (io.tReset.valid)
  val enqueueFire = io.hPort.toHost.hValid && !queueFull && io.tReset.valid

  // Dequeue firing condition.
  // 1. the host is ready to accept tokens (io.hPort.fromHost.hReady)
  val dequeueFire = io.hPort.fromHost.hReady

  val targetReset = enqueueFire & io.tReset.bits
  // Reset buffers with target reset
  for ((_, x) <- outBufs) {
    val queue = x._1
    queue.reset := reset || targetReset
  }

  // We consume instrumentation data from the target and don't send data to target
  // yet, so just mark fromHost as always ready.
  io.hPort.fromHost.hValid := true.B

  // Tokens from the target are consumed with enqueue firing condition
  io.hPort.toHost.hReady := enqueueFire
  // Target reset tokens are also consumed with enqueue firing condition
  io.tReset.ready := enqueueFire

  // Connect queues.
  for ((fieldName, (queue, blocking)) <- outBufs) {
    // Enqueue side.
    queue.io.enq.bits := target(fieldName)
    // Drain-only mode: don't accept new enqueues
    val drain_only = Wire(Bool())
    // Data should be enqueued with firing condition and if not in drain-only mode
    queue.io.enq.valid := enqueueFire && !io.tReset.bits && !drain_only

    // Dequeue side.
    val data_ready = Wire(Bool())
    // Dequeue when host is ready and sends data_ready.
    queue.io.deq.ready := data_ready && dequeueFire

    // Generate memory mapped registers for data buffer.
    // Extra registers:
    // - blocking: Set to true if this queue being full shouldn't block the target.
    // - drain_only: Set to true if this queue should only accept dequeues but not enqueues.
    genROReg(queue.io.deq.bits, s"${fieldName}_data")
    genROReg(!queue.io.enq.ready, s"${fieldName}_full")
    genROReg(queue.io.deq.valid, s"${fieldName}_valid")
    genWORegInit(blocking, s"${fieldName}_blocking", true.B)
    genWORegInit(drain_only, s"${fieldName}_drain_only", false.B)
    Pulsify(genWORegInit(data_ready, s"${fieldName}_ready", false.B), pulseLength = 1)
  }

  override def genHeader(base: BigInt, sb: StringBuilder) {
    super.genHeader(base, sb)
    import CppGenerationUtils._

    // Generate all_ready()
    sb.append(s"#define ${widgetNamePrefix}_all_ready() \\\n")
    for (fieldName <- outBufs.keys) {
      val data_ready = s"${widgetNamePrefix}_${fieldName}_ready"
      sb.append(s"  write(${data_ready}, 1);\\\n")
    }
    sb.append(";\n")

    // Generate drain_only()
    sb.append(s"#define ${widgetNamePrefix}_drain_only(value) \\\n")
    for (fieldName <- outBufs.keys) {
      val regname = s"${widgetNamePrefix}_${fieldName}_drain_only"
      sb.append(s"  write(${regname}, value);\\\n")
    }
    sb.append(";\n")

    // Generate blocking()
    sb.append(s"#define ${widgetNamePrefix}_blocking(value) \\\n")
    for (fieldName <- outBufs.keys) {
      val regname = s"${widgetNamePrefix}_${fieldName}_blocking"
      sb.append(s"  write(${regname}, value);\\\n")
    }
    sb.append(";\n")
  }

  genCRFile()
}
