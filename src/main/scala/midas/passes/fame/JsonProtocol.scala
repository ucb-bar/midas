//See LICENSE for license details.

package midas.passes.fame

import midas.widgets.SerializableEndpointAnnotation

import firrtl.annotations._

import scala.util.{Try, Failure}

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, writePretty}

object JsonProtocol {
  import firrtl.annotations.JsonProtocol._

  val fameChannelInfoClasses = Seq(classOf[PipeChannel], classOf[DecoupledForwardChannel])
  /** Construct Json formatter for annotations */
  def jsonFormat(tags: Seq[Class[_]]) = {
    Serialization.formats(FullTypeHints(tags.toList)).withTypeHintFieldName("class") +
      new TransformClassSerializer + new NamedSerializer + new CircuitNameSerializer +
      new ModuleNameSerializer + new ComponentNameSerializer + new TargetSerializer +
      new GenericTargetSerializer + new CircuitTargetSerializer + new ModuleTargetSerializer +
      new InstanceTargetSerializer + new ReferenceTargetSerializer + new TransformSerializer  +
      new LoadMemoryFileTypeSerializer
  }

  /** Serialize annotations to a String for emission */
  def serialize(annos: Seq[Annotation]): String = serializeTry(annos).get

  def serializeTry(annos: Seq[Annotation]): Try[String] = {
    val tags = annos.map(_.getClass).distinct
    val endpointTags = annos.collect({
      case SerializableEndpointAnnotation(_, _, _, epKey) => epKey.getClass
    }).distinct ++ fameChannelInfoClasses

    implicit val formats = jsonFormat(tags ++ endpointTags)
    Try(writePretty(annos))
  }

  def deserialize(in: JsonInput): Seq[Annotation] = deserializeTry(in).get

  def deserializeTry(in: JsonInput): Try[Seq[Annotation]] = Try({
    val parsed = parse(in)
    val annos = parsed match {
      case JArray(objs) => objs
      case x => throw new InvalidAnnotationJSONException(
        s"Annotations must be serialized as a JArray, got ${x.getClass.getSimpleName} instead!")
    }
    // Gather classes so we can deserialize arbitrary Annotations
    val classes = annos.map({
      case JObject(("class", JString(c)) :: tail) => c
      case obj => throw new InvalidAnnotationJSONException(s"Expected field 'class' not found! $obj")
    }).distinct
    val loaded = classes.map(Class.forName(_).asInstanceOf[Class[_ <: Annotation]]) ++ fameChannelInfoClasses

    implicit val formats = jsonFormat(loaded)
    read[List[Annotation]](in)
  }).recoverWith {
    // Translate some generic errors to specific ones
    case e: java.lang.ClassNotFoundException =>
      Failure(new AnnotationClassNotFoundException(e.getMessage))
    case e: org.json4s.ParserUtil.ParseException =>
      Failure(new InvalidAnnotationJSONException(e.getMessage))
  }.recoverWith { // If the input is a file, wrap in InvalidAnnotationFileException
    case e => in match {
      case FileInput(file) =>
        Failure(new InvalidAnnotationFileException(file, e))
      case _ => Failure(e)
    }
  }
}
