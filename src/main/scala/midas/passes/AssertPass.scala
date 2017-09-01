package midas
package passes

import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import Utils._
import strober.passes.{StroberMetaData, postorder}
import java.io.{File, FileWriter, Writer}

class AssertPass(dir: File) extends firrtl.passes.Pass {
  override def name = "[midas] Assert Pass"
  type Asserts = collection.mutable.HashMap[String, (Int, String)]
  type Messages = collection.mutable.HashMap[Int, String]
  type AssertModSet = collection.mutable.HashSet[String]

  val asserts = collection.mutable.HashMap[String, Asserts]()
  val messages = collection.mutable.HashMap[String, Messages]()
  val ports = collection.mutable.HashMap[String, Port]()

  private def synAsserts(mname: String,
                         namespace: Namespace)
                        (s: Statement): Statement =
    s map synAsserts(mname, namespace) match {
      case s: Stop if s.ret != 0 =>
        val idx = asserts.size
        val name = namespace newName s"assert_$idx"
        asserts(mname)(s.en.serialize) = idx -> name
        DefNode(s.info, name, s.en)
      case s => s
    }

  private def findMessages(mname: String)
                          (s: Statement): Statement =
    s map findMessages(mname) match {
      case s: Print => asserts(mname) get s.en.serialize match {
        case Some((idx, str)) =>
          assert(s.args.isEmpty)
          println(s"$idx, $str => ${s.string.serialize}")
          messages(mname)(idx) = s.string.serialize
          EmptyStmt
        case None => s
      }
      case s => s
    }

  private def transform(meta: StroberMetaData)
                       (m: DefModule): DefModule = {
    val namespace = Namespace(m)
    asserts(m.name) = new Asserts
    messages(m.name) = new Messages
    (m map synAsserts(m.name, namespace)
       map findMessages(m.name)) match {
      case m: Module =>
        val children = (meta.childInsts(m.name) foldRight Seq[(String, Port)]())(
          (x, res) => ports get meta.instModMap(x -> m.name) match {
            case None    => res
            case Some(p) => res :+ (x -> p)
          }
        )
        val width = asserts(m.name).size + ((children foldLeft 0)(
          (res, x) => res + firrtl.bitWidth(x._2.tpe).toInt))
        if (width == 0) m else {
          val tpe = UIntType(IntWidth(width))
          val port = Port(NoInfo, namespace.newName("midasAsserts"), Output, tpe)
          val stmt = Connect(NoInfo, WRef(port.name), cat(
            (children map (x => wsub(wref(x._1), x._2.name))) ++
            (asserts(m.name).values.toSeq sortWith (_._1 > _._1) map (x => wref(x._2)))))
          ports(m.name) = port
          m.copy(ports = m.ports :+ port, body = Block(Seq(m.body, stmt)))
        }
      case m: ExtModule => m
    }
  }

  def dump(writer: Writer, meta: StroberMetaData, mod: String, path: String) {
    asserts(mod).values.toSeq sortWith (_._1 < _._1) foreach { case (idx, _) =>
      writer write s"[module: $mod, path: $path]\n"
      writer write (messages(mod)(idx) replace ("""\n""", "\n"))
      writer write "0\n"
    }
    meta.childInsts(mod) foreach { child =>
      dump(writer, meta, meta.instModMap(child, mod), s"${path}.${child}")
    }
  }

  def run(c: Circuit) = {
    val meta = StroberMetaData(c)
    val mods = postorder(c, meta)(transform(meta))
    val f = new FileWriter(new File(dir, s"${c.main}.asserts"))
    dump(f, meta, c.main, c.main)
    f.close
val cc = c copy (modules = mods)
println(cc.serialize)
cc
}
}
