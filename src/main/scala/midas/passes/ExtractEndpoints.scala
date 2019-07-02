// See LICENSE for license details.

package midas.passes

import midas.targetutils.EndpointAnnotation

import firrtl._
import firrtl.annotations.{CircuitName, ModuleName}
import firrtl.ir._
import firrtl.Mappers._
import Utils._
import java.io.{File, FileWriter, StringWriter}

private[passes] class EndpointExtraction extends firrtl.Transform {

  override def name = "[MIDAS] Endpoint Extraction"
  def inputForm = LowForm
  def outputForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit
    val endpointMTs = state.annotations.collect({
      case EndpointAnnotation(mT) => mT
    })
    endpointMTs foreach println
    state
  }
}
