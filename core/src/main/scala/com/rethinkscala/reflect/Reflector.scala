package com.rethinkscala.reflect

/** Created with IntelliJ IDEA.
 *  User: keyston
 *  Date: 6/16/13
 *  Time: 12:39 PM
 *  To change this template use File | Settings | File Templates.
 */

import java.lang.reflect.{ ParameterizedType, Type, Field }
import com.fasterxml.jackson.databind.{ DeserializationFeature, ObjectMapper }
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.annotation.PropertyAccessor
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility

object Reflector {

  private[this] val classFields = new Memo[Class[_], Seq[Field]]
  private[this] val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)
  mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
  mapper.setVisibility(PropertyAccessor.FIELD,Visibility.ANY)

  def fields(a: AnyRef): Seq[Field] = fields(a.getClass)

  def fields(c: Class[_]): Seq[Field] = classFields(c, _ match {
    case c: Class[_] => c.getDeclaredFields.toSeq.filterNot(_.isSynthetic).take(ctorParams(c)).map {
      field =>
        field.setAccessible(true)
        field
    }

  })

  private def ctorParams(c: Class[_]) = c.getConstructors()(0).getParameterTypes.size

  def toMap(value: Any) = fromJson[Map[String, Any]](toJson(value))

  def toJson(value: Any): String = {
    import java.io.StringWriter
    val writer = new StringWriter()
    mapper.writeValue(writer, value)
    writer.toString
  }

  def fromJson[T: Manifest](value: String): T =
    mapper.readValue(value, typeReference[T])

  private[this] def typeReference[T: Manifest] = new TypeReference[T] {
    override def getType = typeFromManifest(manifest[T])
  }

  private[this] def typeFromManifest(m: Manifest[_]): Type = {
    if (m.typeArguments.isEmpty) {
      m.runtimeClass
    } else new ParameterizedType {
      def getRawType = m.runtimeClass

      def getActualTypeArguments = m.typeArguments.map(typeFromManifest).toArray

      def getOwnerType = null
    }
  }

}
