package com.rethinkscala

import java.lang.reflect.{ Type, ParameterizedType }
import com.fasterxml.jackson.databind.{ DeserializationFeature, ObjectMapper }
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.`type`.TypeReference
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.Some
import com.rethinkscala.reflect.Reflector
import com.rethinkscala.Document
;

object Translate {
  def translate[In, Out](implicit ct: Manifest[Out]): Translate[In, Out] = new BaseTranslate[In, Out] {}

}
trait Translate[In, Out] {

  def write(value: Out): Any
  def read(value: In, json: String, term: Term)(implicit ct: Manifest[Out]): Out

}

trait WithConversion[In, Out] {

  def convert(value: In, json: String)(implicit ct: Manifest[Out]): Out
}
trait WithIterableConversion[Out] extends WithConversion[Iterable[Map[String, _]], Iterable[Out]] {
  def convert(value: Iterable[Map[String, _]], json: String)(implicit ct: Manifest[Out]) = Reflector.fromJson[Iterable[Out]](json)
}

trait MapConversion[Out] extends WithConversion[Map[String, Any], Out]

trait BinaryConversion extends MapConversion[Boolean] {
  val resultField: String

  def convert(value: Map[String, Any], json: String)(implicit mf: Manifest[Boolean]): Boolean = value.get(resultField).getOrElse(0) == 1
}

trait DocumentConversion[Out <: Document] extends MapConversion[Out] {

  def convert(value: Map[String, Any], json: String)(implicit ct: Manifest[Out]): Out =
    Reflector.fromJson[Out](json)

}

trait BaseTranslate[In, Out] extends Translate[In, Out] {

  type Conversion = WithConversion[In, Out]

  def write(value: Out): Any = value

  def read(value: In, json: String, term: Term)(implicit ct: Manifest[Out]): Out = {

    val isDocument = classOf[Document] isAssignableFrom ct.runtimeClass
    term match {
      case c: Conversion => c.convert(value, json)

      case _             => if(isDocument)Reflector.fromJson[Out](json) else value.asInstanceOf[Out]

    }

  }
}

