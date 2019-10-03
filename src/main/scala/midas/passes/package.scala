package midas

import firrtl._
import firrtl.ir._
import firrtl.passes.Pass
import firrtl.Mappers._
import firrtl.WrappedExpression._

import annotations._
import TargetToken.{Instance, OfModule}
import firrtl.Utils.BoolType
import firrtl.transforms.BlackBoxInlineAnno

import scala.language.implicitConversions


package object passes {

  trait WrappedComponent {
    val decl: Statement
    val assigns: Statement
    val ref: Expression
  }

  case class SignalInfo(decl: Statement, assigns: Statement, ref: Expression) extends WrappedComponent

  object PassThru {
    def apply(source: WRef)(implicit ns: Namespace): SignalInfo = apply(source, source.name)
    def apply(source: WRef, name: String)(implicit ns: Namespace): SignalInfo = {
      val decl = DefWire(NoInfo, ns.newName(source.name), source.tpe)
      val ref = WRef(decl)
      SignalInfo(decl, Connect(NoInfo, WRef(decl), source), ref)
    }
  }

  object InstanceInfo {
    def apply(m: DefModule)(implicit ns: Namespace): InstanceInfo = {
      val inst = fame.Instantiate(m, ns.newName(m.name))
      InstanceInfo(inst, Block(Nil), WRef(inst))
    }
  }

  case class InstanceInfo(decl: WDefInstance, assigns: Block, ref: WRef) extends WrappedComponent {
    def addAssign(s: Statement): InstanceInfo = {
      copy(assigns = Block(assigns.stmts :+ s))
    }
    def connect(pName: String, rhs: Expression): InstanceInfo = {
      addAssign(Connect(NoInfo, WSubField(ref, pName), rhs))
    }
    def connect(lhs: Expression, pName: String): InstanceInfo = {
      addAssign(Connect(NoInfo, lhs, WSubField(ref, pName)))
    }
  }

  object DefineAbstractClockGate extends Transform with FunctionalPass[CircuitName] {
    val blackboxName = "AbstractClockGate"
    val blackbox = ExtModule(
      info = NoInfo,
      name = blackboxName,
      ports =  Seq(
        Port(NoInfo, "I", Input, ClockType),
        Port(NoInfo, "CE", Input, BoolType),
        Port(NoInfo, "O", Output, ClockType)),
      defname = blackboxName,
      params = Nil)

    val impl =
      s"""module ${blackboxName}(
          |  input      I,
          |  input      CE,
          |  output reg O
          |);
          |  /* verilator lint_off COMBDLY */
          |  reg enable;
          |  always @(posedge I)
          |    enable <= CE;
          |  assign O = (I & enable);
          |  /* verilator lint_on COMBDLY */
          |endmodule
          |""".stripMargin

    def analyze(cs: CircuitState): CircuitName = CircuitName(cs.circuit.main)
    def transformer(cName: CircuitName) = { c: Circuit => c.copy(modules = c.modules :+ blackbox) }
    def annotater(cName: CircuitName) = {
      anns: AnnotationSeq =>
        val blackboxAnn = BlackBoxInlineAnno(ModuleName(blackboxName, cName), s"${blackboxName}.v", impl)
        anns :+ blackboxAnn
    }
  }

  object OrElseIdentity {
    def apply[T](f: PartialFunction[T, T]): T => T = {
      f.orElse({ case x => x }: PartialFunction[T, T])
    }
  }

  object ModuleTransformer {
    def apply(f: PartialFunction[DefModule, DefModule]): Circuit => Circuit = {
      c => c mapModule OrElseIdentity(f)
    }
  }

  object StatementTransformer {
    def apply(f: PartialFunction[Statement, Statement]): Circuit => Circuit = {
      val fTotal = OrElseIdentity(f)
      ModuleTransformer { case m => m mapStmt { s => fTotal(s mapStmt fTotal) } }
    }
  }

  object ExpressionTransformer {
    def apply(f: PartialFunction[Expression, Expression]): Circuit => Circuit = {
      val fTotal = OrElseIdentity(f)
      StatementTransformer { case s => s mapExpr { e => fTotal(e mapExpr fTotal) } }
    }
  }

  object ReplaceExpression {
    type ReplMap = Map[WrappedExpression, Expression]
    private def onExpr(repls: ReplMap)(e: Expression): Expression = repls.getOrElse(we(e), e map onExpr(repls))
    def apply(repls: ReplMap)(s: Statement): Statement = s map apply(repls) map onExpr(repls)
  }

  trait FunctionalPass[T] {
    def inputForm: CircuitForm = UnknownForm
    def outputForm: CircuitForm = UnknownForm
    def updateForm(i: CircuitForm): CircuitForm = outputForm match {
      case UnknownForm => i
      case _ => outputForm
    }

    def analyze(cs: CircuitState): T
    def preTransformCheck(analysis: T): Unit = ()
    def transformer(analysis: T): Circuit => Circuit
    def annotater(analysis: T): AnnotationSeq => AnnotationSeq
    def renamer(analysis: T): Option[RenameMap] = None

    final def execute(input: CircuitState): CircuitState = {
      val analysis = analyze(input)
      val outputCircuit = transformer(analysis)(input.circuit)
      val outputAnnos = annotater(analysis)(input.annotations)
      CircuitState(outputCircuit, updateForm(input.form), outputAnnos, renamer(analysis))
    }
  }

  trait NoAnalysisPass extends FunctionalPass[Unit] {
    final def analyze(cs: CircuitState): Unit = ()
    final def transformer(analysis: Unit): Circuit => Circuit = transformer
    final def annotater(analysis: Unit): AnnotationSeq => AnnotationSeq = annotater
    final override def renamer(analysis: Unit): Option[RenameMap] = renamer

    val transformer: Circuit => Circuit
    val annotater: AnnotationSeq => AnnotationSeq = identity[AnnotationSeq](_)
    def renamer: Option[RenameMap] = None
  }

}
