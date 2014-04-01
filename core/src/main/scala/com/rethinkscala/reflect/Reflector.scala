package com.rethinkscala.reflect


import scala.collection.JavaConverters._

import com.rethinkscala.Term
import com.fasterxml.jackson.databind.introspect.ClassIntrospector.MixInResolver

/** Created with IntelliJ IDEA.
  * User: keyston
  * Date: 6/16/13
  * Time: 12:39 PM
  * To change this template use File | Settings | File Templates.
  */

import java.lang.reflect.{ParameterizedType, Type, Field}
import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.annotation.PropertyAccessor
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility
import com.fasterxml.jackson.annotation.JsonInclude.Include
import com.fasterxml.jackson.module.scala.introspect.ScalaClassIntrospector

object Reflector {

  import java.io.StringWriter

  private[this] val classFields = new Memo[Class[_], Seq[Field]]
  private[rethinkscala] var mapper = new ObjectMapper()

  mapper.registerModule(RethinkModule)
  mapper.setSerializationInclusion(Include.NON_NULL);
  mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
  mapper.setVisibility(PropertyAccessor.FIELD, Visibility.PUBLIC_ONLY)
  mapper.setDefaultTyping(new RethinkTypeResolverBuilder)

  //mapper.setAnnotationIntrospector()

  def fields(a: AnyRef): Seq[Field] = fields(a.getClass)


  // lazy val spi = new ScalaPropertyIntrospector(mapper.getSerializationConfig.getAnnotationIntrospector)

  private val introspector = ScalaClassIntrospector

  private[this] val mixinResolver = new MixInResolver {
    def findMixInClassFor(cls: Class[_]) = mapper.findMixInClassFor(cls)
  }

  def fields(c: Class[_]): Seq[Field] = classFields(c, {

    case c: Class[_] if classOf[Term] isAssignableFrom c => c.getDeclaredFields.toSeq.filterNot(_.isSynthetic).take(ctorParams(c)).map {
      field =>
        field.setAccessible(true)
        field


    }
    case c: Class[_] =>
      val i = introspector.forCreation(mapper.getDeserializationConfig, mapper.constructType(c), mixinResolver)
      i.findProperties().asScala.filter(_.hasConstructorParameter).map(f => {
        val field = c.getDeclaredField(f.getName)
        field.setAccessible(true)
        field
      }
      )


  })

  /* def fields(c: Class[_]): Seq[Field] = classFields(c, _ match {

     case c: Class[_] => BeanIntrospector(c).properties.view.filter(_.param.isDefined).foreach {
       pd =>
         val name = pd.name
         val pn = _getPropertyName(pd)


         val explName = pn.optMap(_.getSimpleName).map(_.orIfEmpty(name))

         //add the constructor param (if present)
         //call to pd.param.get is safe due to filter above
         val cp = pd.param.get
         ctors.find(_.getAnnotated == cp.constructor).foreach {
           annotatedConstructor =>
             _addFieldCtor(name, annotatedConstructor.getParameter(cp.index), explName)
         }
     }


  })  */

  private def ctorParams(c: Class[_]) = c.getConstructors()(0).getParameterTypes.size

  def toMap(value: Any) = fromJson[Map[String, Any]](toJson(value))

  def toJson(value: Any): String = {

    val writer = new StringWriter()
    mapper.writeValue(writer, value)
    writer.toString
  }

  def fromJson[T: Manifest](value: String): T =
    mapper.readValue(value, typeReference[T])

  private[this] def typeReference[T: Manifest] = new TypeReference[T] {
    override def getType = typeFromManifest(manifest[T])
  }

  private[rethinkscala] def typeFromManifest(m: Manifest[_]): Type = {
    if (m.typeArguments.isEmpty) {
      m.runtimeClass
    } else new ParameterizedType {
      def getRawType = m.runtimeClass

      def getActualTypeArguments = m.typeArguments.map(typeFromManifest(_)).toArray

      def getOwnerType = null
    }
  }

  /*
  import scala.collection.JavaConverters._
  class ScalaPropertyIntrospector(classDef:Class[_],ai: AnnotationIntrospector) {

    require(ai != null, "Argument ai must be non-null")

    private [this] val _ctors = classDef.getConstructors.asScala
    private [this] val _fields = classDef.getFields.asScala
    private [this] val _methods = classDef.memberMethods().asScala

    def findNameForSerialization(prop: PropertyDescriptor): Option[PropertyName] = {
      prop match {
        case PropertyDescriptor(name, optParam, Some(f), _, _) => {
          val annotatedParam = optParam.flatMap { cp =>
            _ctors.find(_.getAnnotated == cp.constructor).map(_.getParameter(cp.index))
          }
          val annotatedField = _fields.find(_.getMember == f)
          val paramName = annotatedParam.optMap(ai.findNameForDeserialization(_))
          val fieldName = annotatedField.optMap(ai.findNameForSerialization(_))
          fieldName orElse paramName
        }

        case PropertyDescriptor(name, _, None, Some(g), _) =>
          _methods.find(_.getMember == g).optMap(ai.findNameForSerialization(_))

        case _ => None
      }
    }

    def findNameForDeserialization(prop: PropertyDescriptor): Option[PropertyName] = {
      prop match {
        case PropertyDescriptor(name, optParam, Some(f), _, _) => {
          val annotatedParam = optParam.flatMap { cp =>
            _ctors.find(_.getAnnotated == cp.constructor).map(_.getParameter(cp.index))
          }
          val annotatedField = _fields.find(_.getMember == f)
          val paramName = annotatedParam.optMap(ai.findNameForDeserialization(_))
          val fieldName = annotatedField.optMap(ai.findNameForDeserialization(_))
          fieldName orElse paramName
        }

        case PropertyDescriptor(name, _, None, _, Some(s)) =>
          _methods.find(_.getMember == s).optMap(ai.findNameForDeserialization(_))

        case _ => None
      }
    }

  }
       */

}
