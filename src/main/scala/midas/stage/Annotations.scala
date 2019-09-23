// See LICENSE for license details.

package midas.stage

import firrtl.options.{StageOption, ShellOption, HasShellOptions}
import firrtl.annotations.NoTargetAnnotation

case class GoldenGateInputAnnotationFileAnnotation(file: String) extends NoTargetAnnotation

object GoldenGateInputAnnotationFileAnnotation extends HasShellOptions {

  val options = Seq(
    new ShellOption[String](
      longOption = "golden-gate-annotation-file",
      toAnnotationSeq = (a: String) => Seq(GoldenGateInputAnnotationFileAnnotation(a)),
      helpText = "An input annotation file, with extended serialization/deserialization support for Golden Gate annotations",
      shortOption = Some("ggaf"),
      helpValueName = Some("<file>") ) )

}
