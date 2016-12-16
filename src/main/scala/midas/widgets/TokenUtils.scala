package midas
package widgets

import chisel3._
import chisel3.core.Direction
import uncore.axi4.{AXI4Bundle, AXI4BundleParameters}
import junctions.NastiIO

import scala.reflect.runtime.universe._
import java.io.{File, PrintWriter}

// This file contains a collection of classes and objects for the generation of
// CPP for managing tokens

trait MidasToken {
  def cName = this.getClass.getSimpleName
  def getFields = this.getClass.getDeclaredFields.map({ field =>
      field.setAccessible(true)
      (field.getName -> field.get(this))
    }).sortWith { _._1 < _._1}
}

trait AXI4Token extends MidasToken {
  def dataWidth: Int
  def idWidth: Int
}
case class AXI4SToken(idWidth: Int, dataWidth: Int) extends AXI4Token
case class AXI4MToken(idWidth: Int, addrWidth: Int, dataWidth: Int) extends AXI4Token


object TokenFactory {

  def tokenizeAXI4(b: AXI4Bundle): (AXI4Token, AXI4Token) = {
      val idWidth = b.ar.bits.id.getWidth
      val addrWidth = b.ar.bits.addr.getWidth
      val dataWidth = b.r.bits.data.getWidth
      val mToken = AXI4MToken(idWidth, addrWidth, dataWidth)
      val sToken = AXI4SToken(idWidth, dataWidth)
      if (b.aw.valid.dir == Direction.Output) (mToken, sToken) else (sToken, mToken)
    }

  def tokenizeAXI4(b: NastiIO): (AXI4Token, AXI4Token) = {
      val idWidth = b.ar.bits.id.getWidth
      val addrWidth = b.ar.bits.addr.getWidth
      val dataWidth = b.r.bits.data.getWidth
      val mToken = AXI4MToken(idWidth, addrWidth, dataWidth)
      val sToken = AXI4SToken(idWidth, dataWidth)
      if (b.aw.valid.dir == Direction.Output) (mToken, sToken) else (sToken, mToken)
    }

  def apply[T <: Bundle](b: T): (MidasToken, MidasToken) = b match {
    case b: NastiIO => tokenizeAXI4(b)
    case b: AXI4Bundle => tokenizeAXI4(b)
    case _ => throw new RuntimeException("Unsupported bundle type: " ++ b.getClass.getName)
  }

  def apply[T <: Bundle](b: T, desiredDir: Direction): MidasToken =
    if (desiredDir == Direction.Output) apply(b)._1 else apply(b)._2
}

object TokenUtils {
  def getStorageType(data: Element): String = data match {
    case b: Bool => "uint32_t"
    case u: UInt => {
      if (u.getWidth <= 32) "uint32_t"
      else if (u.getWidth <= 64) "uint64_t"
      else "biguint_t"
    }
    // TODO: Figure out what makes sense here
    case i: SInt => ???
    case _ => throw new RuntimeException("Unrecognized element type")
  }

  // Given a chisel data type, return the total bitwidth of all elements of the desired direction
  def getTokenWidth(data: Data, desiredDir: Direction): Int = data match {
    case e: Element => {
      require (e.dir != Direction.Unspecified, "Directions on all leaf nodes must be specified")
      if (e.dir == desiredDir) e.getWidth else 0 }
    //I'm unsure about this, specifically around vecs of bundles
    case v: Vec[_] => v map { getTokenWidth(_, desiredDir) } reduce {_ + _}
    case b: Bundle => b.elements map { t => getTokenWidth(t._2, desiredDir) } reduce {_ + _}
  }

  // Returns true if a bundle has at least one element of the desired direction
  def hasElementsOfDesiredDirection(bundle: Bundle, desiredDir: Direction): Boolean =
    getTokenWidth(bundle, desiredDir) != 0

  // This function emits a CPP class that encodes a Chisel type
  // used to automate the generate of the intial token definitions
  def genTokenType(bundle: Bundle, desiredDir: Direction): String =  {
    val sb = new CStringBuilder
    val bundleTypeSuffix = "_b"
    val token = TokenFactory(bundle, desiredDir)
    // Private members that indicate the width of variable width fields
    val tokenParams = token.getFields.unzip._1
   
    // Recurses down the bundle creating structs for every new bundle encountered and instantiating storage types for all elements
    def genInnerType(name: String, value: Data): Unit = value match {
      case e: Element => if (e.dir == desiredDir) {
        sb.appendln("%s %s;".format(getStorageType(e), name))
      }
      case b: Bundle => {
        if (hasElementsOfDesiredDirection(b, desiredDir)) {
          sb.appendln(s"struct ${name}${bundleTypeSuffix} {")
          sb.indent()
          b.elements foreach { case (name, value) => genInnerType(name, value)}
          sb.unindent()
          sb.appendln("};")
          sb.appendln(s"${name}${bundleTypeSuffix} ${name};")
         }
      }
      case v: Vec[_] => ???
      case _ => throw new RuntimeException("Unrecognized chisel type")
    }

    def genConstructor(): Unit = {
      val argList = tokenParams map { "int " ++ _ } mkString(", ")
      val initializerList = tokenParams map { name => s"$name{$name}" } mkString(", ")
      sb.appendln("%s(%s) : %s {};".format(token.cName, argList, initializerList))
    }
    def genDestructor(): Unit = {
      sb.appendln("~%s() {};".format(token.cName))
    }

    def genGetters(): Unit = {
      tokenParams foreach { field => sb.appendln("int get_%s(void){ return %s; };".format(field, field)) }
    }

    sb.appendln("class %s: public MidasToken {".format(token.cName))
    sb.indent()
    sb.appendln("private:")
    sb.indent()
    tokenParams foreach { param => sb.appendln("int %s;".format(param)) }
    sb.append("\n")
    sb.unindent()
    sb.appendln("public:")
    sb.indent()
    // Generate the data fields
    bundle.elements foreach { case (name: String, value: Data) => genInnerType(name, value) }
    // Generate public methods
    sb.appendln("\n")
    genConstructor
    genDestructor
    genGetters()
    sb.unindent(2)
    sb.appendln("};\n")
    sb.toString
  }

  def writeTokenTypesToFile(fw: PrintWriter, bundle: Bundle): Unit = {
    fw.write(genTokenType(bundle, Direction.Output))
    fw.write(genTokenType(bundle, Direction.Input))
  }

  // A higher order recursive function, that recurses down a bundle and applies
  // it's function argument to all of the leaves of the bundle (Elements)
  def traverseBundle(bundleHier: Seq[String], element: Data)(
      f: (Seq[String], Element) => Unit ): Unit = element match {
    case e: Element => f(bundleHier, e)
    case v: Vec[_] => ??? // I'm lazy...
    case b: Bundle => b.elements foreach { case (name: String, element: Data) =>
      traverseBundle(bundleHier ++ Seq(name), element)(f)}
  }

  // Given a bundle, this method generates a C function to unpack an array
  // into a token aka. fromBits()
  def genFromBits(name: String, bundle: Bundle, desiredDir: Direction): String = {
    // This var stores our position within the bitpack
    // Since elements are stored in reverse order we need to start from the MSB
    var offset = getTokenWidth(bundle, desiredDir) - 1
    val token = TokenFactory(bundle, desiredDir)
    val sb = new CStringBuilder

    // Initalizes a leaf field of the C++ token, bumping the offset as the traversal procedes
    def unpackElement(bundleHier: Seq[String], element: Element): Unit = {
      if (element.dir == desiredDir) {
        scala.Predef.assert(offset - element.getWidth + 1 >= 0)
        val unpackMethod = {
          if (element.getWidth <= 32) "extract_uint32"
          else if (element.getWidth <= 64) "extract_uint64"
          else "extract"
        }

        sb.appendln("token->%s = bits.%s(%d, %d);".format(
          bundleHier.mkString("."),
          unpackMethod,
          offset - element.getWidth + 1,
          offset))
        offset -= element.getWidth
      }
    }

    def unpackElements(bundle: Bundle) = traverseBundle(Seq(), bundle)(unpackElement)

    val bitStorageType = "biguint_t&"
    val constructorArgs = token.getFields.unzip._2.mkString(", ")

    // Now generate the factory function
    sb.appendln("%s* %s_fromBits(%s bits) {".format(token.cName, name, bitStorageType))
    sb.indent()
    sb.appendln("%s* token = new %s(%s);".format(token.cName, token.cName, constructorArgs))
    unpackElements(bundle)
    sb.appendln("return token;")
    sb.unindent()
    sb.appendln("};")
    sb.toString
  }

  // This preforms the reverse operation of genFromBits
  def genToBits(name: String, bundle: Bundle, desiredDir: Direction): String = {
    var offset = getTokenWidth(bundle, desiredDir) - 1
    val sb = new CStringBuilder
    val token = TokenFactory(bundle, desiredDir)

    def packElement(bundleHier: Seq[String], element: Element): Unit = {
      if (element.dir == desiredDir) {
        sb.appendln("bits.set_bits(%d, %d, %s);".format(
          offset - element.getWidth + 1,
          offset,
          bundleHier.mkString(".")))
        offset -= element.getWidth
      }
    }

    def packElements(bundle: Bundle) = traverseBundle(Seq("token"), bundle)(packElement)

    val numWords = offset/32 + 1

    sb.appendln(s"biguint_t* %s_toBits(%s& token) {".format(name, token.cName))
    sb.indent()
    sb.appendln("size_t num_words = %d;".format(numWords))
    sb.appendln("uint32_t bits[num_words] = {0};")
    sb.appendln("biguint_t * pack = new biguint_t(bits, num_words);")
    packElements(bundle)
    // We probably don't need to use biguint?
    sb.appendln("return pack;")
    sb.unindent()
    sb.appendln("};")
    sb.toString
  }
}

object TokenDefinitionGenerator extends App {
  import TokenUtils.writeTokenTypesToFile

  class DummyContext extends Module {
    val io = IO( new Bundle {} )
    val fw = new PrintWriter(new File("tokens.h"))
    writeTokenTypesToFile(fw, (AXI4Bundle(AXI4BundleParameters(64, 128, 32))))
    fw.close()
  }

  chisel3.Driver.elaborate(() => new DummyContext)

}
