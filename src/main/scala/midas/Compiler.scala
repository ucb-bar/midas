package midas

import chisel3.{Data, Bundle}
import firrtl.ir.Circuit
import firrtl.CompilerUtils.getLoweringTransforms
import firrtl.passes.memlib._
import java.io.{File, FileWriter, Writer}

// Compiler in Midas Passes
class InlineCompiler extends firrtl.Compiler {
  def emitter = new firrtl.MiddleFirrtlEmitter
  def transforms = getLoweringTransforms(firrtl.ChirrtlForm, firrtl.MidForm)
}

// Compiler for Midas Transforms
private class MidasCompiler(dir: File, io: Data)(implicit param: config.Parameters) extends firrtl.Compiler {
  def emitter = new firrtl.MiddleFirrtlEmitter
  def transforms = getLoweringTransforms(firrtl.ChirrtlForm, firrtl.MidForm) ++ Seq(
    new InferReadWrite,
    new ReplSeqMem,
    new passes.MidasTransforms(dir, io)
  )
}

// Compilers to emit proper verilog
private class VerilogCompiler(confFile: File, macroFile: File) extends firrtl.Compiler {
  def emitter = new passes.MidasVerilogEmitter(confFile, macroFile)
  def transforms = getLoweringTransforms(firrtl.HighForm, firrtl.LowForm) :+ (
    new firrtl.LowFirrtlOptimization)
}

object MidasCompiler {
  def apply(chirrtl: Circuit, io: Data, dir: File)(implicit p: config.Parameters): Circuit = {
    val confFile = new File(dir, s"${chirrtl.main}.conf")
    val macroFile = new File(dir, s"${chirrtl.main}.macros.v")
    val annotations = new firrtl.AnnotationMap(Seq(
      firrtl.passes.memlib.InferReadWriteAnnotation(chirrtl.main),
      firrtl.passes.memlib.ReplSeqMemAnnotation(s"-c:${chirrtl.main}:-o:$confFile"),
      passes.MidasAnnotation(chirrtl.main, confFile)
    ))
    // val writer = new FileWriter(new File("debug.ir"))
    val writer = new java.io.StringWriter
    val midas = new MidasCompiler(dir, io) compile (
      firrtl.CircuitState(chirrtl, firrtl.ChirrtlForm, Some(annotations)), writer)
    // writer.close
    // firrtl.Parser.parse(writer.toString)
    val verilog = new FileWriter(new File(dir, s"${midas.circuit.main}.v"))
    val result = new VerilogCompiler(confFile, macroFile) compile (
      firrtl.CircuitState(midas.circuit, firrtl.HighForm), verilog)
    verilog.close
    result.circuit
  }

  def apply[T <: chisel3.experimental.RawModule](w: => T, dir: File)(implicit p: config.Parameters): Circuit = {
    dir.mkdirs
    lazy val target = w
    val chirrtl = firrtl.Parser.parse(chisel3.Driver.emit(() => target))

    class RCBundle extends Bundle {
        val clock = target.getPorts(0).id.cloneType
        val reset = target.getPorts(1).id.cloneType
        val mem_axi4 = target.getPorts(2).id.cloneType
        val serial = target.getPorts(3).id.cloneType
        val uarts = target.getPorts(4).id.cloneType
        val net = target.getPorts(5).id.cloneType
        val bdev = target.getPorts(6).id.cloneType

        override def cloneType = new RCBundle().asInstanceOf[this.type]
    }

    val rcbundle = new RCBundle
    apply(chirrtl, rcbundle, dir)
  }
}
