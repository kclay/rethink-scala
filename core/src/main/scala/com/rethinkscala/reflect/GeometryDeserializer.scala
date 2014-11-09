package com.rethinkscala.reflect

import com.fasterxml.jackson.annotation.JsonTypeInfo.As
import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.core.{JsonToken, JsonParser}
import com.fasterxml.jackson.databind.jsontype.TypeIdResolver
import com.fasterxml.jackson.databind.jsontype.impl.TypeDeserializerBase
import com.fasterxml.jackson.databind.{BeanProperty, DeserializationContext, JavaType, JsonDeserializer}
import com.rethinkscala._
import com.fasterxml.jackson.databind.deser.std.StdScalarDeserializer

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 11/3/2014
 * Time: 11:46 AM 
 */


case class GeometryExtractor[T](coordinates: List[T])

case class PolygonExtractor(coordinates: List[List[Point]]) {
  def value = Polygon(coordinates.head)
}


object GeometryDeserializer {
  val classOfPoint = classOf[(Double, Double)]
  val classOfPolygon = classOf[Polygon]


}


object PointDeserializer extends StdScalarDeserializer[Point](classOf[Point]) {
  override def deserialize(jp: JsonParser, p2: DeserializationContext) = {

    jp.nextToken() // [
    val long = jp.getDoubleValue
    jp.nextToken()
    val lat = jp.getDoubleValue
    jp.nextToken() //[


    Point(long, lat)
  }
}

object PolygonDeserializer extends StdScalarDeserializer[Polygon](classOf[Polygon]) {
  val classOfPolygonExtractor = Reflector.typeReference[PolygonExtractor]

  override def deserialize(jp: JsonParser, p2: DeserializationContext) = jp
    .readValueAs(classOfPolygonExtractor)
    .asInstanceOf[PolygonExtractor].value
}

/*
case class GeometryDeserializer[T <: Geometry](baseType: JavaType
                                                   ,
                                                   idRes: TypeIdResolver
                                                   ,
                                                   typePropertyName: String
                                                   ,
                                                   typeIdVisible: Boolean
                                                   ,
                                                   defaultImpl: Class[_])(implicit mf: Manifest[T]) extends
TypeDeserializerBase(baseType, idRes, typePropertyName, typeIdVisible, defaultImpl) {

  import GeometryDeserializer._

  override def deserializeTypedFromAny(jp: JsonParser, ctxt: DeserializationContext) = deserialize(jp, ctxt)

  override def deserializeTypedFromScalar(jp: JsonParser, ctxt: DeserializationContext) = deserialize(jp, ctxt)

  override def deserializeTypedFromArray(jp: JsonParser, ctxt: DeserializationContext) = deserialize(jp, ctxt)

  override def deserializeTypedFromObject(jp: JsonParser, ctxt: DeserializationContext) = deserialize(jp, ctxt)

  override def getTypeInclusion = As.WRAPPER_OBJECT


  override def forProperty(prop: BeanProperty) = this


  type Result = GeometryExtractor[T]


  def deserialize(jp: JsonParser, ctxt: DeserializationContext): AnyRef = {


    val results = baseType.getRawClass match {
      case `classOfPoint` => Option(jp.readValueAs(classOf[(Double, Double)])).map(l => Point(l._1, l._2))
      case `classOfPolygon` => {
       jp.readValueAs(polygonExtractor)

      }
    }
    results
  }
}

*/