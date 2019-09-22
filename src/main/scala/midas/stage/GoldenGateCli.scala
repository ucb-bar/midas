// See LICENSE for license details.

package midas.stage

import firrtl.options.Shell

trait GoldenGateCli { this: Shell =>
  parser.note("Golden Gate Compiler Options")
  Seq( firrtl.stage.FirrtlFileAnnotation,
       firrtl.stage.OutputFileAnnotation,
       firrtl.stage.InfoModeAnnotation,
       firrtl.stage.FirrtlSourceAnnotation,
       firrtl.EmitCircuitAnnotation,
       firrtl.EmitAllModulesAnnotation )
    .map(_.addOptions(parser))

  //phases.DriverCompatibility.TopNameAnnotation.addOptions(parser)
  //phases.DriverCompatibility.EmitOneFilePerModuleAnnotation.addOptions(parser)
}
