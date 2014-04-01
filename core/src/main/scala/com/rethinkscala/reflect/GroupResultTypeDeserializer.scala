package com.rethinkscala.reflect

import com.fasterxml.jackson.databind.jsontype.impl.TypeDeserializerBase
import com.fasterxml.jackson.databind.{JavaType, DeserializationContext, BeanProperty}
import com.fasterxml.jackson.core.{JsonToken, JsonParser}
import com.fasterxml.jackson.databind.jsontype.TypeIdResolver
import com.fasterxml.jackson.annotation.JsonTypeInfo.As
import com.fasterxml.jackson.core.JsonToken._
import com.rethinkscala.GroupResultRecord
import com.rethinkscala.GroupResult
import scala.collection.mutable.Builder
import scala.collection.Seq


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 3/29/14
 * Time: 7:03 PM
 *
 */
case class GroupResultTypeDeserializer(baseType: JavaType
                                       ,
                                       idRes: TypeIdResolver
                                       ,
                                       typePropertyName: String
                                       ,
                                       typeIdVisible: Boolean
                                       ,
                                       defaultImpl: Class[_]) extends
TypeDeserializerBase(baseType, idRes, typePropertyName, typeIdVisible, defaultImpl) with
ReqlTypeDeserializer[GroupResult[_]] {


  override val dataStartKey = "data"

  override val reqlTypeValue =  "GROUPED_DATA"


  override def deserializeTypedFromAny(jp: JsonParser, ctxt: DeserializationContext) = deserialize(jp, ctxt)

  override def deserializeTypedFromScalar(jp: JsonParser, ctxt: DeserializationContext) = deserialize(jp, ctxt)

  override def deserializeTypedFromArray(jp: JsonParser, ctxt: DeserializationContext) = deserialize(jp, ctxt)

  override def deserializeTypedFromObject(jp: JsonParser, ctxt: DeserializationContext) = deserialize(jp, ctxt)

  override def getTypeInclusion = As.WRAPPER_OBJECT



  override def forProperty(prop: BeanProperty) = this


  type ReductionType = Seq[Map[String, Any]]
  val reductionType = Reflector.typeFromManifest(implicitly[Manifest[ReductionType]])


  val mf = implicitly[Manifest[Seq[GroupResultRecord[_]]]]


  override def resolve(token: JsonToken)(implicit ctx: DeserializationContext, jp: JsonParser): Any = {

    token match {
      case START_ARRAY => {

        val t1 = next

        var results:Option[Any] = None

        do {

          val innerType = Option(_baseType.containedType(0)).map(f=>{
            next
            f
          }).getOrElse(_baseType)
          val deser = ctx.findContextualValueDeserializer(innerType, null)


          val groupValue = deser.deserialize(jp, ctx)

          results = buildGroupRecord(groupValue)


        } while (next != END_ARRAY)

        results.getOrElse(null)


      }
      case _ => throw ctx.mappingException("Invalid json must be and array")


    }
  }

  val builder: Builder[_, Seq[_]] = Seq.newBuilder[GroupResultRecord[_]]

  def add[T](record: GroupResultRecord[T])(implicit jp: JsonParser) = {

    val b = builder.asInstanceOf[Builder[GroupResultRecord[T], Seq[GroupResultRecord[T]]]]
    b += record
    if (next == END_ARRAY) Some(buildResult(b.result())) else None
  }

  def buildResult[T](records: Seq[GroupResultRecord[T]]) = new GroupResult[T](records)


  def buildGroupRecord[T](groupValue: T)(implicit ctx: DeserializationContext, jp: JsonParser) = {
    next
    val reduction: ReductionType = ctx.readValue(jp, ctx.getTypeFactory.constructType(reductionType))

    add(new GroupResultRecord[T](groupValue, reduction))
  }

}
