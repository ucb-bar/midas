// See LICENSE for license details.

package midas.stage

import firrtl.AnnotationSeq
import firrtl.options.{Phase, PhaseManager, PreservesAll, Shell, Stage, StageMain}
import firrtl.options.phases.DeletedWrapper
import firrtl.options.Viewer.view

import java.io.{StringWriter, PrintWriter}

class GoldenGateStage extends Stage with PreservesAll[Phase] {
  val shell: Shell = new Shell("goldengate") with GoldenGateCli

  private val phases: Seq[Phase] =
    Seq(
        new GoldenGateGetIncludes,
        new firrtl.stage.phases.AddDefaults,
        new firrtl.stage.phases.AddImplicitEmitter,
        new firrtl.stage.phases.Checks,
        new firrtl.stage.phases.AddCircuit,
        new firrtl.stage.phases.AddImplicitOutputFile,
        new midas.stage.GoldenGateCompilerPhase,
        new firrtl.stage.phases.WriteEmitted )
      .map(DeletedWrapper(_))


  def run(annotations: AnnotationSeq): AnnotationSeq = phases.foldLeft(annotations)((a, f) => f.transform(a))
}

object GoldenGateMain extends StageMain(new GoldenGateStage)
//class GoldenGateMain {
//
//  /** The main function that serves as this stage's command line interface.
//    * @param args command line arguments
//    */
//  final def main(args: Array[String]): Unit = try {
//    stage.execute(args, Seq.empty)
//  } catch {
//    case a: OptionsException =>
//      StageUtils.dramaticUsageError(a.message)
//      System.exit(1)
//  }
//
//}
