package midas
package passes

import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.passes.createMask
import firrtl.passes.MemPortUtils.memPortField
import firrtl.Utils.{one, kind, create_exps}
import midas.passes.Utils._

case class MemConf(
  name: String,
  depth: BigInt,
  width: BigInt,
  readers: Seq[String],
  writers: Seq[String],
  readwriters: Seq[String],
  maskGran: BigInt)

object MemConfReader {
  sealed trait ConfField
  case object Name extends ConfField
  case object Depth extends ConfField
  case object Width extends ConfField
  case object Ports extends ConfField
  case object MaskGran extends ConfField
  type ConfFieldMap = Map[ConfField, String]
  // Read a conf file generated by [[firrtl.passes.ReplSeqMems]]
  def apply(conf: java.io.File): Seq[MemConf] = {
    def parse(map: ConfFieldMap, list: List[String]): ConfFieldMap = list match {
      case Nil => map
      case "name" :: value :: tail => parse(map + (Name -> value), tail)
      case "depth" :: value :: tail => parse(map + (Depth -> value), tail)
      case "width" :: value :: tail => parse(map + (Width -> value), tail)
      case "ports" :: value :: tail => parse(map + (Ports -> value), tail)
      case "mask_gran" :: value :: tail => parse(map + (MaskGran -> value), tail)
      case field :: tail => firrtl.Utils.error(s"Unknown field $field")
    }
    io.Source.fromFile(conf).getLines.toSeq map { line =>
      val map = parse(Map[ConfField, String](), (line split " ").toList)
      val ports = map(Ports) split ","
      MemConf(map(Name), BigInt(map(Depth)), BigInt(map(Width)),
        ports filter (_ == "read"),
        ports filter (p => p == "write" || p == "mwrite"),
        ports filter (p => p == "rw" || p == "mrw"),
        map get MaskGran map (BigInt(_)) getOrElse (BigInt(map(Width))))
    }
  }
}

private class SeqMem(info: Info, mem: MemConf) extends DefMemory(
  info,
  mem.name,
  if (mem.maskGran == mem.width) UIntType(IntWidth(mem.width))
  else VectorType(UIntType(IntWidth(mem.maskGran)), (mem.width / mem.maskGran).toInt),
  mem.depth.toInt,
  1, // writeLatency
  1, // readLatency
  mem.readers.indices map (i => s"R$i"),
  mem.writers.indices map (i => s"W$i"),
  mem.readwriters.indices map (i => s"RW$i")) {
  val hasMaskPort = mem.width != mem.maskGran
}

private[passes] class ToSeqMems(conf: java.io.File) extends firrtl.passes.Pass {
  override def name = "[strober] To SeqMems"

  lazy val seqMems = (MemConfReader(conf) map (m => m.name -> m)).toMap

  type Netlist = collection.mutable.HashMap[String, Expression]

  def portMap(mem: SeqMem, ports: Seq[Port]): Seq[(String, Expression)] =
    (mem.readers flatMap { r =>
      val tpe = (ports find (_.name == r)).get.tpe
      (create_exps(wsub(wref(r, tpe), "data")) zip
       create_exps(memPortField(mem, r, "data")))
    }) ++ (mem.writers flatMap { w =>
      val tpe = (ports find (_.name == w)).get.tpe
      Seq(memPortField(mem, w, "data") -> wsub(wref(w, tpe), "data"),
          memPortField(mem, w, "mask") -> wsub(wref(w, tpe), "mask"))
    }) ++ (mem.readwriters flatMap { rw =>
      val tpe = (ports find (_.name == rw)).get.tpe
      (create_exps(wsub(wref(rw, tpe), "rdata")) zip
       create_exps(memPortField(mem, rw, "rdata"))) ++
      Seq(memPortField(mem, rw, "wdata") -> wsub(wref(rw, tpe), "wdata"),
          memPortField(mem, rw, "wmask") -> wsub(wref(rw, tpe), "wmask"))
    }) map { case (x, y) => x.serialize -> y }

  def setMask(mem: SeqMem) = {
    (mem.writers map (p => Connect(NoInfo, memPortField(mem, p, "mask"), one))) ++
    (mem.readwriters map (p => Connect(NoInfo, memPortField(mem, p, "wmask"), one)))
  }

  def replaceInsts(netlist: Netlist, ports: Seq[Port])(s: Statement): Statement =
    s map replaceInsts(netlist, ports) match {
      case s: WDefInstance => seqMems get s.name match {
        case None => s
        case Some(bb) =>
          val mem = new SeqMem(s.info, bb)
          if (mem.hasMaskPort) {
            netlist ++= portMap(mem, ports)
            mem
          } else Block(mem +: setMask(mem))
      }
      case s => s
    }

  def replaceExps(netlist: Netlist)(s: Statement): Statement =
    s map replaceExps(netlist) match {
      case s: Connect => netlist get s.loc.serialize match {
        case None => s
        case Some(e) => s.copy(expr=e)
      }
      case s => s
    }

  def onMod(m: DefModule): DefModule = {
    val netlist = new Netlist
    val mx = m map replaceInsts(netlist, m.ports)
    if (netlist.isEmpty) mx else mx map replaceExps(netlist)
  }

  def run(c: Circuit) = c.copy(modules =
    c.modules filterNot (seqMems contains _.name) map onMod)
}
