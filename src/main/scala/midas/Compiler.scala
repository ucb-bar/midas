package midas

import java.io.{File, FileWriter, Writer}

import scala.collection.immutable.ListMap

import freechips.rocketchip.config.Parameters
import chisel3.{Data, Bundle, Record}
import firrtl.ir.Circuit
import firrtl.CompilerUtils.getLoweringTransforms
import firrtl.passes.memlib._
import barstools.macros._

// Compiler for Midas Transforms
private class MidasCompiler(dir: File, targetPorts: Record)(implicit param: Parameters) 
    extends firrtl.Compiler {
  def emitter = new firrtl.LowFirrtlEmitter
  def transforms = getLoweringTransforms(firrtl.ChirrtlForm, firrtl.MidForm) ++ Seq(
    new InferReadWrite,
    new ReplSeqMem) ++
    getLoweringTransforms(firrtl.MidForm, firrtl.LowForm) ++ Seq(
    new passes.MidasTransforms(dir, targetPorts))
}

// Compilers to emit proper verilog
private class VerilogCompiler extends firrtl.Compiler {
  def emitter = new firrtl.VerilogEmitter
  def transforms = getLoweringTransforms(firrtl.HighForm, firrtl.LowForm) :+ (
    new firrtl.LowFirrtlOptimization)
}

// A convenience class that populates a Record with a port list, returned by Module.getPorts 
class TargetPortRecord(portList: Seq[chisel3.internal.firrtl.Port]) extends Record {
  val elements = ListMap((for (port <- portList) yield {
      (port.id.instanceName -> port.id.chiselCloneType)
    }):_*)
  override def cloneType = new TargetPortRecord(portList).asInstanceOf[this.type]
}

object TargetPortRecord {
  def apply(mod: chisel3.experimental.RawModule) = new TargetPortRecord(mod.getPorts)
}

object MidasCompiler {
  // Generates the verilog and memory map for a MIDAS simulation
  // Accepts: An elaborated chisel circuit, record that mirrors its I/O,
  // an output directory, and technology library
  def apply(
      chirrtl: Circuit,
      targetPorts: Record,
      dir: File,
      lib: Option[File])
     (implicit p: Parameters): Circuit = {
    val conf = new File(dir, s"${chirrtl.main}.conf")
    val json = new File(dir, s"${chirrtl.main}.macros.json")
    val annotations = new firrtl.AnnotationMap(Seq(
      InferReadWriteAnnotation(chirrtl.main),
      ReplSeqMemAnnotation(s"-c:${chirrtl.main}:-o:$conf"),
      passes.MidasAnnotation(chirrtl.main, conf, json, lib),
      MacroCompilerAnnotation(chirrtl.main, MacroCompilerAnnotation.Params(
        json.toString, lib map (_.toString), CostMetric.default, true))))
    val writer = new java.io.StringWriter
    val midas = new MidasCompiler(dir, targetPorts) compile (
      firrtl.CircuitState(chirrtl, firrtl.ChirrtlForm, Some(annotations)), writer)
    val verilog = new FileWriter(new File(dir, s"FPGATop.v"))
    val result = new VerilogCompiler compile (
      firrtl.CircuitState(midas.circuit, firrtl.HighForm), verilog)
    verilog.close
    result.circuit
  }

  // Unlike above, elaborates the target locally, before constructing the target IO Record.
  def apply[T <: chisel3.experimental.RawModule](w: => T, dir: File, libFile: Option[File])
      (implicit p: Parameters): Circuit = {
    dir.mkdirs
    lazy val target = w
    val chirrtl = firrtl.Parser.parse(chisel3.Driver.emit(() => target))
    val targetPorts = TargetPortRecord(target)
    apply(chirrtl, targetPorts, dir, libFile)
  }
}

