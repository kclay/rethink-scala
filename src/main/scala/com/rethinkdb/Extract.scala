package com.rethinkdb

import java.lang.reflect.{Type, ParameterizedType}
import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.`type`.TypeReference;


object Extract{
  def extract[In, Out](implicit mf: Manifest[Out]): Extract[In, Out] = new BaseExtract[In, Out] {}
  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)
  mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES,false)

  def write(value: Any): String = {
    import java.io.StringWriter
    val writer = new StringWriter()
    mapper.writeValue(writer, value)
    writer.toString
  }

  def read[T: Manifest](value: String) : T =
    mapper.readValue(value, typeReference[T])

  private [this] def typeReference[T: Manifest] = new TypeReference[T] {
    override def getType = typeFromManifest(manifest[T])
  }

  private [this] def typeFromManifest(m: Manifest[_]): Type = {
    if (m.typeArguments.isEmpty) { m.erasure }
    else new ParameterizedType {
      def getRawType = m.erasure
      def getActualTypeArguments = m.typeArguments.map(typeFromManifest).toArray
      def getOwnerType = null
    }
  }
}
trait Extract[In, Out] {

  def extract(value: In, term: Term)(implicit mf: Manifest[Out]): Out

}

trait WithConversion[In, Out] {

  def convert(value: In)(implicit ct: Manifest[Out]): Out
}

trait MapConversion[Out] extends WithConversion[Map[String, Any], Out]

trait BinaryConversion extends MapConversion[Boolean] {
  val resultField: String

  def convert(value: Map[String, Any])(implicit mf:Manifest[Boolean]): Boolean = value.get(resultField).getOrElse(0) == 1
}

trait DocumentConversion[Out <: Document] extends MapConversion[Out] {

  import scala.reflect.runtime.{universe=>ru}

  /*
  val pn = new CachingParanamer(new BytecodeReadingParanamer)

  def fill[T](m: Map[String,Any])(]) = for {
    ctor <- ct.runtimeClass.getDeclaredConstructors.filter(m => m.getParameterTypes.forall(classOf[String]==)).headOption
    parameters = pn.lookupParameterNames(ctor)
  } yield ctor.newInstance(parameters.map(m): _*).asInstanceOf[T]
  private def fields(a: AnyRef) = {

    val f = a.getClass.getDeclaredFields
    f.toSeq.filterNot(_.isSynthetic).take(numConstructorParams(a)).map { field =>
      field.setAccessible(true)
      field
    }
  }
  private def numConstructorParams(a: AnyRef) = a.getClass.getConstructors()(0).getParameterTypes.size
   */





  import Extract.{read,write}

  //
  def convert(value: Map[String, Any])(implicit mf: Manifest[Out]): Out ={
   /* val ctor = ru.typeOf[Out].declaration(ru.nme.CONSTRUCTOR).asMethod
    val mapping = ctor.paramss.head.map{ p=>

      val mapTo = p.annotations.find(_.tpe == fieldAnnotationType).map{
        f=>Some(f.scalaArgs.head.productElement(0).asInstanceOf[ru.Constant].value.asInstanceOf[String])
      }
      (p.name.decoded,mapTo)
    }*/





 /*

    implicit val fmt =mapping.collect{
      case (from:String,to:String)=>new CustomSerializer[Out](
        serializer = renameTo(to,from),
        deserializer = renameFrom(to,from)
      )
    }.foldLeft(DefaultFormats:Formats)(_ + _)
   */

   read[Out](write(value))
  }

}

trait BaseExtract[In, Out] extends Extract[In, Out] {

  type Conversion = WithConversion[In, Out]

  def extract(value: In, term: Term)(implicit mf: Manifest[Out]): Out = {
    def cast(v: Any): Out = v.asInstanceOf[Out]
    term match {
      case c: Conversion => cast(c.convert(value))
      case _             => cast(value)
    }

  }
}

