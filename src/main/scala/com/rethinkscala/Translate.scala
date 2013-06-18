package com.rethinkscala

import java.lang.reflect.{ Type, ParameterizedType }
import com.fasterxml.jackson.databind.{ DeserializationFeature, ObjectMapper }
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.`type`.TypeReference
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.Some;

object Translate {
  def translate[In, Out](implicit ct: Manifest[Out]): Translate[In, Out] = new BaseTranslate[In, Out] {}
  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)
  mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)

  def write(value: Any): String = {
    import java.io.StringWriter
    val writer = new StringWriter()
    mapper.writeValue(writer, value)
    writer.toString
  }

  def read[T: Manifest](value: String): T =
    mapper.readValue(value, typeReference[T])

  private[this] def typeReference[T: Manifest] = new TypeReference[T] {
    override def getType = typeFromManifest(manifest[T])
  }

  private[this] def typeFromManifest(m: Manifest[_]): Type = {
    if (m.typeArguments.isEmpty) { m.runtimeClass }
    else new ParameterizedType {
      def getRawType = m.runtimeClass
      def getActualTypeArguments = m.typeArguments.map(typeFromManifest).toArray
      def getOwnerType = null
    }
  }
}
trait Translate[In, Out] {

  def write(value: Out): Any
  def read(value: In, json: String, term: Term)(implicit ct: Manifest[Out]): Out

}

trait WithConversion[In, Out] {

  def convert(value: In, json: String)(implicit ct: Manifest[Out]): Out
}

trait MapConversion[Out] extends WithConversion[Map[String, Any], Out]

trait BinaryConversion extends MapConversion[Boolean] {
  val resultField: String

  def convert(value: Map[String, Any], json: String)(implicit mf: Manifest[Boolean]): Boolean = value.get(resultField).getOrElse(0) == 1
}

trait DocumentConversion[Out <: Document] extends MapConversion[Out] {

  def convert(value: Map[String, Any], json: String)(implicit ct: Manifest[Out]): Out = {

    Translate.read[Out](json)

    // convert0[Out](value)

  }

}

trait BaseTranslate[In, Out] extends Translate[In, Out] {

  type Conversion = WithConversion[In, Out]

  def write(value: Out): Any = value

  def read(value: In, json: String, term: Term)(implicit ct: Manifest[Out]): Out = {
    def cast(v: Any): Out = v.asInstanceOf[Out]
    term match {
      case c: Conversion => cast(c.convert(value, json))
      case _             => cast(value)
    }

  }
}

