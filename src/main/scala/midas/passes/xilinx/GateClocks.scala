package midas.passes.xilinx

import firrtl._
import annotations._

/**
  * A pass that gates clocks using BUFGCE selectable global clock buffers. Each clock that is marked with a ClockGateAnnotation 
  * is gated using the corresponding gate signal from the annotation. This pass is part of the midas.passes.xilinx subpackage due to its use of vendor-specific macros.
  */

object GlobalClockBuffer {
  val name = "BUFGCE"
  val ports = Seq(Port(NoInfo, "I", Input, ClockType),
                  Port(NoInfo, "CE", Input, Utils.BoolType),
                  Port(NoInfo, "O", Output, ClockType))
  val module = ExtModule(NoInfo, name, ports, name, Nil)
  def instance(i: String) = WDefInstance(NoInfo, i, name, Utils.module_type(module))
}

class ReplaceExpression(repls: Map[WrappedExpression, Expression]) {
  private val onExpr: (Expression => Expression) = { e => repls.getOrElse(we(e), e map onExpr) }
  private val onStmt: (Statement => Statement) = { s => s map onStmt map onExpr }
  def apply(e: Expression) = onExpr(e)
  def apply(s: Statement) = onStmt(s)
}

object GateClocks extends Transform {

  case class ClockBufferInfo(buf: WDefInstance, conns: Block, ungated: WrappedExpression)

  def buildExpression(base: Expression, token: TargetToken): Expression = token match {
    case Index(value: Int) => WSubIndex(base, value, UnknownType, UnknownFlow)
    case Field(value: String) => WSubField(base, value, UnknownType, UnknownFlow)
  }

  def toExpression(rt: ReferenceTarget) = rt.component.foldLeft(WRef(rt.ref))(buildExpression)

  def execute(cs: CircuitState): CircuitState = {
    val moduleNamespaces = cs.circuit.modules.view.map(m => m.name -> Namespace(m)).toMap
    val (cgAnnos, otherAnnos) = cs.annotations.partition(_.isInstanceOf[ClockGateAnnotation])
    val bufferData = cgAnnos.map {
      case ClockGateAnnotation(clock, gate) =>
        require(clock.module == gate.module && clock.isLocal && gate.isLocal)
        val ns = moduleNamespaces(clock.module)
        val ungated = toExpression(clock)
        val buf = GlobalClockBuffer.instance(ns.newName(s"${ungated}_gate"))
        val clockConn = Connect(NoInfo, WSubField(WRef(buf), "I"), ungated)
        val ceConn = Connect(NoInfo, WSubField(WRef(buf), "CE"), toExpression(gate))
	(OfModule(clock.module), ClockBufferInfo(buf, Block(Seq(clockConn, ceConn)), WrappedExpression(ungated)))
    }

    val buffersByModule = bufferData.groupBy(_._1).view.mapValues(_._2).toMap
    val transformedModules = cs.circuit.modules.map {
      case m: Module if buffersByModule.contains(m) =>
        val buffers = buffersByModule(m)
        val repls = buffers.map { b => b.ungated -> WRef(b.buf) }
        m.copy(body = Block(Seq((buffers.map(_.buf) :+ m.body.map(replaceExp(repls))) ++ buffers.map(_.conns))))
      case m => m
    }

    cs.copy(circuit = cs.circuit.copy(modules = transformedModules), annotations = otherAnnos)
  }

}

case class FAMEChannelPortsAnnotation(
  localName: String,
  ports: Seq[ReferenceTarget]) extends Annotation {
  def update(renames: RenameMap): Seq[Annotation] = {
    val renamer = RTRenamer.exact(renames)
    Seq(FAMEChannelPortsAnnotation(localName, ports.map(renamer)))
  }
  override def getTargets: Seq[ReferenceTarget] = ports
}

/**
  * An annotation that describes the top-level connectivity of
  * channels on different model instances.
  *
  * @param globalName  a globally unique name for this channel connection
  *
  * @param channelInfo  describes the type of the channel (Wire, Forward/Reverse
  *  Decoupled)
  */
case class FAMEChannelConnectionAnnotation(
  globalName: String,
  channelInfo: FAMEChannelInfo,
  sources: Option[Seq[ReferenceTarget]],
  sinks: Option[Seq[ReferenceTarget]]) extends Annotation {
  def update(renames: RenameMap): Seq[Annotation] = {
    val renamer = RTRenamer.exact(renames)
    Seq(FAMEChannelConnectionAnnotation(globalName, channelInfo.update(renames), sources.map(_.map(renamer)), sinks.map(_.map(renamer))))
  }
  override def getTargets: Seq[ReferenceTarget] = sources.toSeq.flatten ++ sinks.toSeq.flatten
}

/**
  * Describes the type of the channel (Wire, Forward/Reverse
  * Decoupled)
  */
sealed trait FAMEChannelInfo {
  def update(renames: RenameMap): FAMEChannelInfo = this
}


/**
  * Indicates that a channel connection is a pipe with <latency> register stages
  * Setting latency = 0 models a wire
  *
  * TODO: How to handle registers that are reset? Add an Option[RT]?
  */
case class PipeChannel(val latency: Int) extends FAMEChannelInfo

/** 
  * Indicates that a channel connection is the reverse (ready) half of
  * a decoupled target connection. Since the forward half incorporates
  * references to the ready signals, this channel contains no signal
  * references.
  */
case object DecoupledReverseChannel extends FAMEChannelInfo

/**
  * Indicates that a channel connection is the reverse (ready) half of
  * a decoupled target connection.
  * 
  * @param readySink  sink port component of the corresponding reverse channel
  * 
  * @param validSource  valid port component from this channel's sources
  * 
  * @param readySource  source port component of the corresponding reverse channel
  * 
  * @param validSink  valid port component from this channel's sinks
  * 
  * @note  (readySink, validSource) are on one model, (readySource, validSink) on the other
  */
case class DecoupledForwardChannel(
  readySink: Option[ReferenceTarget],
  validSource: Option[ReferenceTarget],
  readySource: Option[ReferenceTarget],
  validSink: Option[ReferenceTarget]) extends FAMEChannelInfo {
  override def update(renames: RenameMap): DecoupledForwardChannel = {
    val renamer = RTRenamer.exact(renames)
    DecoupledForwardChannel(
      readySink.map(renamer),
      validSource.map(renamer),
      readySource.map(renamer),
      validSink.map(renamer))
  }
}


/**
  * Indicates that a particular instance is a FAME Model
  */
case class FAMEModelAnnotation(target: InstanceTarget) extends SingleTargetAnnotation[InstanceTarget] {
  def targets = Seq(target)
  def duplicate(n: InstanceTarget) = this.copy(n)
}


/**
  * Specifies what form of FAME transform should be applied when
  * generated a simulation model from a target module.
  */
abstract class FAMETransformType

/**
  * When tied to a target in a FAMETransformAnnotation, this specifies
  * that the module should be transformed with the basic LI-BDN
  * compliant FAME1 transform.
  */
case object FAME1Transform extends FAMETransformType

/**
  * Indicates that a particular target module from the "AQB" canonical
  * form should be transformed to a FAME model.
  *
  * @param transformType  Describes which variant of the FAME transform
  *  should be applied to the target module. Currently, the
  *  FAME1Transform object is the only value that this can take.
  *
  * @param target  Points to the target module to be transformed. Since
  *  this is a ModuleTarget, all instances at the top level will be
  *  transformed identically.
  */
case class FAMETransformAnnotation(transformType: FAMETransformType, target: ModuleTarget) extends SingleTargetAnnotation[ModuleTarget] {
  def targets = Seq(target)
  def duplicate(n: ModuleTarget) = this.copy(transformType, n)
}

/**
  * Indicates that a particular target instance should be promoted one
  * level in the hierarchy. The specified instance will be pulled out
  * of its parent module and will reside in its "grandparent" module
  * after the PromoteSubmodule transform has run.
  * 
  * @param target  The instance to be promoted. Note that this must
  *  be a *local* instance target, as all instances of the parent
  *  module will be transformed identically.
  */
case class PromoteSubmoduleAnnotation(target: InstanceTarget) extends SingleTargetAnnotation[InstanceTarget] {
  def targets = Seq(target)
  def duplicate(n: InstanceTarget) = this.copy(n)
}

abstract class FAMEGlobalSignal extends SingleTargetAnnotation[ReferenceTarget] {
  val target: ReferenceTarget
  def targets = Seq(target)
  def duplicate(n: ReferenceTarget): FAMEGlobalSignal
}

case class FAMEHostClock(target: ReferenceTarget) extends FAMEGlobalSignal {
  def duplicate(t: ReferenceTarget): FAMEHostClock = this.copy(t)
}

case class FAMEHostReset(target: ReferenceTarget) extends FAMEGlobalSignal {
  def duplicate(t: ReferenceTarget): FAMEHostReset = this.copy(t)
}

abstract class MemPortAnnotation extends Annotation {
  val en: ReferenceTarget
  val addr: ReferenceTarget
}

object ModelReadPort {
  def apply(rpt: ReferenceTarget) =
    new ModelReadPort(
      rpt.field("data"),
      rpt.field("addr"),
      rpt.field("en"))
}

case class ModelReadPort(
  data: ReferenceTarget,
  addr: ReferenceTarget,
  en: ReferenceTarget) extends MemPortAnnotation {
  def update(renames: RenameMap): Seq[Annotation] = {
    val renamer = RTRenamer.exact(renames)
    Seq(ModelReadPort(renamer(data), renamer(addr), renamer(en)))
  }
  override def getTargets: Seq[ReferenceTarget] = Seq(data, addr, en)
}

object ModelWritePort {
  def apply(rpt: ReferenceTarget) =
    new ModelWritePort(
      rpt.field("data"),
      rpt.field("mask"),
      rpt.field("addr"),
      rpt.field("en"))
}

case class ModelWritePort(
  data: ReferenceTarget,
  mask: ReferenceTarget,
  addr: ReferenceTarget,
  en: ReferenceTarget) extends MemPortAnnotation {
  def update(renames: RenameMap): Seq[Annotation] = {
    val renamer = RTRenamer.exact(renames)
    Seq(ModelWritePort(renamer(data), renamer(mask), renamer(addr), renamer(en)))
  }
  override def getTargets: Seq[ReferenceTarget] = Seq(data, mask, addr, en)
}

object ModelReadWritePort {
  def apply(rpt: ReferenceTarget) =
    new ModelReadWritePort(
      rpt.field("wmode"),
      rpt.field("rdata"),
      rpt.field("wdata"),
      rpt.field("wmask"),
      rpt.field("addr"),
      rpt.field("en"))
}

case class ModelReadWritePort(
  wmode: ReferenceTarget,
  rdata: ReferenceTarget,
  wdata: ReferenceTarget,
  wmask: ReferenceTarget,
  addr: ReferenceTarget,
  en: ReferenceTarget) extends MemPortAnnotation {
  def update(renames: RenameMap): Seq[Annotation] = {
    val renamer = RTRenamer.exact(renames)
    Seq(ModelReadWritePort(renamer(wmode), renamer(rdata), renamer(wdata), renamer(wmask), renamer(addr), renamer(en)))
  }
  override def getTargets: Seq[ReferenceTarget] = Seq(wmode, rdata, wdata, wmask, addr, en)
}
