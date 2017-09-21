package midas
package passes

import java.io.{File, FileWriter, StringWriter}

import chisel3.{Record}
import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.Utils.BoolType
import firrtl.passes.LowerTypes.loweredName
import firrtl.Utils.{splitRef, mergeRef, create_exps, gender, module_type}
import Utils._

import midas.core.{SimWrapper}

private[passes] class SimulationMapping(
    targetPorts: Record)
   (implicit param: freechips.rocketchip.config.Parameters) extends firrtl.passes.Pass {
  
  override def name = "[midas] Simulation Mapping"

  private def initStmt(target: String)(s: Statement): Statement =
    s match {
      case s: WDefInstance if s.name == "target" && s.module == "TargetBox" =>
        s copy (module = target) // replace TargetBox with the actual target module
      case s => s map initStmt(target)
    }

  private def init(info: Info, target: String, main: String, tpe: Type)(m: DefModule) = m match {
    case m: Module if m.name == "TargetBoxBundleWrap" =>
      val body = initStmt(target)(m.body)
      val stmts = Seq(
        Connect(NoInfo, wsub(wref("target"), "targetFire"), wref(loweredName(wsub(wref("io"), "fire")))),
        Connect(NoInfo, wsub(wref("target"), "daisyReset"), wref("reset", BoolType))) ++
      (if (!param(EnableSnapshot)) Nil
       else {
         val ports = (m.ports map (p => p.name -> p)).toMap
         create_exps(wsub(wref("target", tpe), "daisy")) map { e =>
           val io = WRef(loweredName(mergeRef(wref("io"), splitRef(e)._2)))
           ports(io.name).direction match {
             case Input  => Connect(NoInfo, e, io)
             case Output => Connect(NoInfo, io, e)
           }
         }
       })
      Some(m copy (info = info, body = Block(body +: stmts)))
    case m: Module => Some(m)
    case m: ExtModule => None
  }

  // Performs simulation mapping. Here, we:
  //
  // 1) In a seperate chisel circuit, generate the token queues on the ports of the target design
  // and bind them to the ports of a black box (TargetBox) with the same IO as the transfomed target
  //
  // 2) Link the transformed target with this simulation wrapper, by combining their firrtl 
  // circuits and replacing instances of TargetBox with the real target
  def run(c: Circuit) = {
    lazy val sim = new SimWrapper(targetPorts)
    val chirrtl = Parser parse (chisel3.Driver emit (() => sim))
    val annotations = new AnnotationMap(Nil)
    val writer = new StringWriter
    val targetType = module_type((c.modules find (_.name == c.main)).get)
    // val writer = new FileWriter(new File("SimWrapper.ir"))
    val circuit = renameMods((new LowFirrtlCompiler compile (
      CircuitState(chirrtl, ChirrtlForm), writer)).circuit, Namespace(c))
    val modules = c.modules ++ (circuit.modules flatMap
      init(c.info, c.main, circuit.main, targetType))
    // writer.close
    new WCircuit(circuit.info, modules, circuit.main, sim.io)
  }
}
