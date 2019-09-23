// See LICENSE for license details.

package midas.stage

import midas._

import firrtl.ir.Circuit
import firrtl.{Transform, CircuitState, AnnotationSeq}
import firrtl.annotations.{Annotation}
import firrtl.options.{Phase, TargetDirAnnotation}
import firrtl.stage.{FirrtlCircuitAnnotation}
import firrtl.CompilerUtils.getLoweringTransforms
import firrtl.passes.memlib._
import freechips.rocketchip.config.{Parameters, Field}
import java.io.{File, FileWriter, Writer}
import logger._

class GoldenGateCompilerPhase extends Phase {

  def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val allCircuits = annotations.collect({ case FirrtlCircuitAnnotation(circuit) => circuit })
    require(allCircuits.size == 1, "Golden Gate can only process a single Firrtl Circuit at a time.")
    val circuit = allCircuits.head

    val io: Seq[(String, chisel3.Data)] = Seq.empty
    val targetTransforms: Seq[Transform] = Seq.empty // Run pre-MIDAS transforms, on the target RTL
    val hostTransforms: Seq[Transform] = Seq.empty   // Run post-MIDAS transformations

    val midasAnnos = Seq(
      InferReadWriteAnnotation
    )
    val targetDir = annotations.collectFirst({
      case TargetDirAnnotation(targetDir) => new File(targetDir)
    }).get

    implicit val p = (new F1Config).alterPartial({ case OutputDir => targetDir })
    val midasTransforms = new passes.MidasTransforms(io)
    val compiler = new MidasCompiler
    val midas = compiler.compile(firrtl.CircuitState(
      circuit, firrtl.HighForm, annotations ++ midasAnnos),
      targetTransforms :+ midasTransforms)

    val postHostTransforms = new HostTransformCompiler().compile(midas, hostTransforms)
    val result = new LastStageVerilogCompiler().compile(postHostTransforms, Seq())
    FirrtlCircuitAnnotation(result.circuit) +: result.annotations
  }
}
