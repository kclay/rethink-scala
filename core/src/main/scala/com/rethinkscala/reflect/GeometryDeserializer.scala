package com.rethinkscala.reflect

import com.fasterxml.jackson.core.{JsonToken, JsonParser}
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.deser.std.StdScalarDeserializer
import com.fasterxml.jackson.databind.node.ArrayNode
import com.rethinkscala.{Point, Polygon, Line, UnknownGeometry, GeometryType}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 11/12/2014
 * Time: 4:36 PM 
 */
case class PolygonExtractor(coordinates: List[List[Point]]) {
  def toPolygon = Polygon(coordinates.head)

  def toLine = Line(coordinates.head)
}

case class LineExtractor(coordinates: List[Point]) {
  def toLine =Line(coordinates)
}

object PointDeserializer extends StdScalarDeserializer[Point](classOf[Point]) {
  val COORDINATES_FIELD = "coordinates"
  override def deserialize(jp: JsonParser, p2: DeserializationContext) = {



    def point =  {

        jp.nextToken()

        val long = jp.getDoubleValue
        jp.nextToken()
        val lat = jp.getDoubleValue

        jp.nextToken() //[
        Point(long, lat)



    }
    if(jp.getCurrentToken == JsonToken.START_ARRAY) point else {
      while(jp.getCurrentToken != JsonToken.START_ARRAY)jp.nextToken()

      val value = point

      while(jp.getCurrentToken != JsonToken.END_OBJECT) jp.nextToken()
      value

    }


  }


}

object UnknownGeometryDeserializer extends StdScalarDeserializer[UnknownGeometry](classOf[UnknownGeometry]) {
  val COORDINATES_FIELD = "coordinates"
  val TYPE_FIELD = "type"

  override def deserialize(jp: JsonParser, p2: DeserializationContext) = {

    val nodes = jp.readValueAsTree[JsonNode]
    val name = nodes.get(TYPE_FIELD).asText()
    val coordinates = nodes.get(COORDINATES_FIELD).asInstanceOf[ArrayNode]
    val geometryType: Option[GeometryType] = Option(name).collect {
      case "Point" => Point(coordinates.get(0).asDouble(), coordinates.get(1).asDouble())
      case "Polygon" | "LineString" =>
        val points = for (i <- 0 to coordinates.size() - 1) yield {
          val node = coordinates.get(i)
            .asInstanceOf[ArrayNode]
          Point(node.get(0).asDouble(), node.get(1).asDouble())
        }
        if (name == "Polygon") Polygon(points) else Line(points)


    }

    geometryType.fold(null.asInstanceOf[UnknownGeometry])(t => new UnknownGeometry(t))

  }
}

object LineDeserializer extends StdScalarDeserializer[Line](classOf[Line]) {
  val typeRefOfPointsExtractor = Reflector.typeReference[LineExtractor]

  override def deserialize(jp: JsonParser, p2: DeserializationContext) = jp
    .readValueAs(typeRefOfPointsExtractor)
    .asInstanceOf[LineExtractor].toLine
}

object PolygonDeserializer extends StdScalarDeserializer[Polygon](classOf[Polygon]) {
  val typeRefOfPointsExtractor = Reflector.typeReference[PolygonExtractor]

  override def deserialize(jp: JsonParser, p2: DeserializationContext) = jp
    .readValueAs(typeRefOfPointsExtractor)
    .asInstanceOf[PolygonExtractor].toPolygon
}