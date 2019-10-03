package midas.passes

import firrtl._
import annotations._

/**
  * An annotation that indicates to downstream clock gating passes that a
  * specific clock in the design should be gated by a specific Boolean signal.
  * 
  * The clock and gate must be local ReferenceTargets with same root module.
  *
  * @param clock The clock to gate
  * @param gate The boolean signal that is used to gate the clock
  */
case class ClockGateAnnotation(clock: ReferenceTarget, gate: ReferenceTarget) extends Annotation {
  require(clock.isLocal && gate.isLocal, "ClockGateAnnotations require local clock and gate targets")
  require(clock.moduleTarget == gate.moduleTarget, "a gated clock and its gate signal must be in the same module")

  def update(renames: RenameMap): Seq[Annotation] = {
    val renamer = RTRenamer.exact(renames)
    Seq(FAMEChannelPortsAnnotation(renamer(clock), renamer(gate)))
  }

  override def getTargets: Seq[ReferenceTarget] = Seq(clock, gate)
}
