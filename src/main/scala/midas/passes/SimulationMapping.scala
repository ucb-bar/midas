package midas
package passes

import midas.core.SimWrapper
import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.Utils.BoolType
import firrtl.passes.LowerTypes.loweredName
import firrtl.Utils.{splitRef, mergeRef, create_exps, module_type, get_field}
import Utils._
import java.io.{File, FileWriter, StringWriter}

private[passes] class SimulationMapping(
    io: chisel3.Data)
   (implicit param: config.Parameters) extends firrtl.passes.Pass {
  
  override def name = "[midas] Simulation Mapping"

  private def initStmt(target: String)(s: Statement): Statement =
    s match {
      case s: WDefInstance if s.name == "target" && s.module == "TargetBox" =>
        s copy (module = target) // replace TargetBox with the actual target module
      case s => s map initStmt(target)
    }

  private def init(info: Info,
                   target: String,
                   main: String,
                   tpe: Type,
                   sim: SimWrapper)
                  (m: DefModule) = m match {
    case m: Module if m.name == main =>
      val body = initStmt(target)(m.body)
      val stmts = Seq(
        Connect(NoInfo, wsub(wref("target"), "targetFire"), wref("fire", BoolType)),
        Connect(NoInfo, wsub(wref("target"), "daisyReset"), wref("reset", BoolType))) ++
      (if (!param(EnableDebug) || sim.numAsserts == 0) Nil
       else Seq(Connect(NoInfo, wref("asserts"), wsub(wref("target"), "midasAsserts")))) ++
      (if (!param(EnablePrint)) Nil
       else sim.printPorts.unzip._1 map { name =>
         val wire = wref(loweredName(wsub(wref("prints"), name)))
         val port = wsub(wsub(wref("target"), "midasPrints"), name)
         Connect(NoInfo, wire, port) }) ++
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

  def run(c: Circuit) = {
    val targetType = module_type((c.modules find (_.name == c.main)).get)
    lazy val sim = new SimWrapper(io)(param alterPartial {
      case core.NumAsserts => try {
        bitWidth(get_field(targetType, "midasAsserts").tpe).toInt
      } catch {
        case e: FIRRTLException => 0
      }
      case core.PrintPorts => try {
        get_field(targetType, "midasPrints").tpe match {
          case BundleType(fs) => fs map (f => f.name -> bitWidth(f.tpe).toInt)
        }
      } catch {
        case e: FIRRTLException => Nil
      }
    })
    val chirrtl = Parser parse (chisel3.Driver emit (() => sim))
    val annotations = new AnnotationMap(Nil)
    val writer = new StringWriter
    // val writer = new FileWriter(new File("SimWrapper.ir"))
    val circuit = renameMods((new LowFirrtlCompiler compile (
      CircuitState(chirrtl, ChirrtlForm), writer)).circuit, Namespace(c))
    val modules = c.modules ++ (circuit.modules flatMap
      init(c.info, c.main, circuit.main, targetType, sim))
    // writer.close
    new WCircuit(circuit.info, modules, circuit.main, sim.io)
  }
}
