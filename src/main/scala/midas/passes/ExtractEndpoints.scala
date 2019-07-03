// See LICENSE for license details.

package midas.passes

import midas.targetutils.EndpointAnnotation
import midas.passes.fame.{PromoteSubmodule, PromoteSubmoduleAnnotation}

import firrtl._
import firrtl.annotations._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.transforms.TopWiring.{TopWiringAnnotation, TopWiringTransform, TopWiringOutputFilesAnnotation}
import firrtl.passes.wiring.{Wiring, WiringInfo}
import Utils._

import scala.collection.mutable
import java.io.{File, FileWriter, StringWriter}

private[passes] class EndpointExtraction extends firrtl.Transform {

  case class EndpointInstance(target: InstanceTarget) extends SingleTargetAnnotation[InstanceTarget] {
    def targets = Seq(target)
    def duplicate(n: InstanceTarget) = this.copy(n)
  }

  override def name = "[MIDAS] Endpoint Extraction"
  def inputForm = MidForm
  def outputForm = MidForm

  private val endpointMods = new mutable.HashSet[String]()
  private val wiringAnnos  = mutable.ArrayBuffer[WiringInfo]()
  private val topPorts     = mutable.ArrayBuffer[Port]()

  // Taken from extract model -- recursively calls PromoteSubmodule until all
  // endpoints live at the top of the module hierarchy
  def promoteEndpoints(state: CircuitState): CircuitState = {
    val anns = state.annotations.flatMap {
      case a @ EndpointInstance(it) if (it.module != it.circuit) => Seq(a, PromoteSubmoduleAnnotation(it))
      case a => Seq(a)
    }
    if (anns.toSeq == state.annotations.toSeq) {
      state
    } else {
      promoteEndpoints((new PromoteSubmodule).runTransform(state.copy(annotations = anns)))
    }
  }

  // Propogate Endpoint ModuleAnnotations to Instance Annotations
  def annotateInstances(state: CircuitState): CircuitState = {
    val c = state.circuit
    val iGraph  =  new firrtl.analyses.InstanceGraph(c)
    // Collect all endpoint modules
    val endpointAnnos = state.annotations.collect({ case EndpointAnnotation(mT) => mT })
    val endpointModules = endpointAnnos.map(_.module)

    // Get a list of all endpoint instances using iGraph
    val endpointInsts: Seq[Seq[WDefInstance]] = endpointModules.flatMap(e => iGraph.findInstancesInHierarchy(e).map(_.reverse))

    // Generate instance annotations to drive promoteEndpoints()
    val instAnnos = endpointInsts.collect({ case endpoint :: parent :: restOfPath =>
      EndpointInstance(InstanceTarget(c.main, parent.module, Nil, endpoint.name, endpoint.module))
    })
    instAnnos foreach println
    state.copy(annotations = state.annotations ++ instAnnos)
  }


  def execute(state: CircuitState): CircuitState = {
    val c = state.circuit
    val circuitNS = Namespace(c)

    // First create a dummy top into which we'll promote endpoints
    val topWrapperName = circuitNS.newName("DummyTop")
    val topInstance = WDefInstance("realTopInst", c.main)
    val topWrapper = Module(NoInfo, topWrapperName, Seq(), Block(Seq(topInstance)))
    val wrappedTopState = state.copy(circuit = c.copy(modules = topWrapper +: c.modules, main = topWrapperName))

    // Annotate all endpoint instances
    val instAnnoedState = annotateInstances(wrappedTopState)

    // Promote all modules that are annotated as endpoints
    val promotedState = promoteEndpoints(instAnnoedState)

    // Propogate endpoint annotations to the IO created on the true top module


    // Remove all endpoint modules and the dummy top wrapper
    val modulesToRemove = endpointMods += topWrapperName
    val prunedCircuit = promotedState.circuit.copy(
      modules = promotedState.circuit.modules.filterNot(m => modulesToRemove(m.name)),
      main = c.main
    )
    assert(1 == 0)
    promotedState.copy(circuit = prunedCircuit)
  }
}
