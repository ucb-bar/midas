package midas
package widgets

import chisel3._
import chisel3.core.Direction

object CConstants {
  val indent = "  "
}

trait CPPLiteral {
  def typeString: String
  def toC: String
}

trait IntLikeLiteral extends CPPLiteral {
  def bitWidth: Int
  def literalSuffix: String
  def value: BigInt
  def toC = value.toString + literalSuffix
}

case class UInt32(value: BigInt) extends IntLikeLiteral {
  def typeString = "unsigned int"
  def bitWidth = 32
  def literalSuffix = ""

  require(bitWidth >= value.bitLength)
}

case class UInt64(value: BigInt) extends IntLikeLiteral {
  def typeString = "uint64_t"
  def bitWidth = 64
  def literalSuffix = "L"

  require(bitWidth >= value.bitLength)
}

case class CStrLit(val value: String) extends CPPLiteral {
  def typeString = "const char* const"
  def toC = "\"%s\"".format(value)
}

// This class wraps a string builder with some private mutable state to
// track the current indentation
class CStringBuilder {
  private var indentDepth = 0
  val sb = new StringBuilder

  def indent(by: Int = 1): Unit = { indentDepth = indentDepth + by }
  def unindent(by: Int = 1): Unit = {
    assert(indentDepth >= by)
    indentDepth = indentDepth - by
  }

  def curIndent = CConstants.indent * indentDepth

  def append[T](toAppend: T): Unit = sb.append(toAppend)

  def appendln[T](toAppend: T): Unit = {
    sb.append(curIndent)
    sb.append(toAppend)
    sb.append("\n")
  }

  override def toString = sb.toString
}

object CppGenerationUtils {
  import CConstants._

  def genArray[T <: CPPLiteral](name: String, values: Seq[T]): String = {
    if ( values.size > 0 ) {
      val prefix = "static %s %s[%d] = {\n".format(values.head.typeString, name, values.size)
      val body = values.tail.foldLeft(indent + values.head.toC)(
       (body: String, value: T) => body + ",\n%s%s".format(indent, value.toC))
      val suffix = "\n};\n"
      prefix + body + suffix
    } else {
      ""
    }
  }

  def genConstStatic[T <: CPPLiteral](name: String, value: T): String =
    "const static %s %s = %s;\n".format(value.typeString, name, value.toC)

  def genMacro[T <: CPPLiteral](name: String, value: T): String =
    "#define %s %s\n".format(name, value.toC)

  def genComment(str: String): String = "// %s\n".format(str)
}
