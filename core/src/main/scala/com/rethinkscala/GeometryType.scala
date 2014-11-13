package com.rethinkscala

import com.fasterxml.jackson.annotation.{JsonUnwrapped, JsonProperty, JsonTypeInfo}
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.annotation.{JsonDeserialize, JsonTypeResolver}
import com.fasterxml.jackson.databind.node.ArrayNode
import com.rethinkscala.ast._
import com.rethinkscala.reflect.{GroupResultDeserializer}
import ql2.Ql2.Term.TermType

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 11/3/2014
 * Time: 12:10 PM 
 */


trait GeometryType {


}


trait GeoSystem extends WrappedValue[String] {

}

object WGS84 extends GeoSystem {
  override val value: String = "WGS84"
}

trait GeoUnit extends WrappedValue[String]

object Meter extends GeoUnit {
  override val value: String = "m"
}

object KiloMeter extends GeoUnit {
  override val value: String = "km"
}

object InternationalMile extends GeoUnit {
  override val value: String = "mi"
}

object NauticalMile extends GeoUnit {
  override val value: String = "nm"
}

object InternationalFoot extends GeoUnit {
  override val value: String = "ft"
}

object UnitSphere extends GeoSystem {
  override val value: String = "unit_sphere"
}

case class Point(long: Double, lat: Double) extends GeometryType with ProducePoint {


  override lazy val args = buildArgs(long, lat)

  override def termType = TermType.POINT


  def this(l: List[Double]) {
    this(l(0), l(1))
  }
}


case class Polygon(points: Seq[Point]) extends GeometryType with ProducePolygon {

  override lazy val args = buildArgs(points: _*)
  override def termType = TermType.POLYGON


}

case class Line(points: Seq[Point]) extends GeometryType with ProduceLine {

  override lazy val args = buildArgs(points: _*)

  override def termType = TermType.LINE
}


final class LineSupport(val line: Line) extends AnyVal {
  def fill = Fill(line)
}

final class SequenceGeoSupport[T <: GeometryType](val seq: ProduceSequence[T]) extends AnyVal {
  def intersects(geo: GeometryType) = Intersects(SequenceIntersect(seq), geo)
}

final class PolygonSupport(val polygon: ProduceGeometry[Polygon]) extends AnyVal {
  def polygonSub(other: Polygon) = PolygonSub(polygon, other)
}

class UnknownGeometry(geo: GeometryType) extends GeometryType {
  def isPoint = geo.isInstanceOf[Point]

  def isPolygon = geo.isInstanceOf[Polygon]

  def isLine = geo.isInstanceOf[Line]

  def isCircle = geo.isInstanceOf[Circle]

  def circle = if (isCircle) Some(geo.asInstanceOf[Circle]) else None

  def polygon = if (isPolygon) Some(geo.asInstanceOf[Polygon]) else None

  def line = if (isLine) Some(geo.asInstanceOf[Line]) else None

  def point = if (isPoint) Some(geo.asInstanceOf[Point]) else None

  //def map(pf:PartialFunction)


}

final class AnyGeoSupport(val any: CastTo) extends AnyVal {

  private def cast[T <: GeometryType](name: String) = any.field(name).asInstanceOf[ProduceGeometry[T]]

  private def as[T <: GeometryType] = any.asInstanceOf[ProduceGeometry[T]]

  def asPoint = as[Point]

  def point(name: String) = cast[Point](name)

  def asLine = as[Line]

  def line(name: String) = cast[Line](name)

  def asPolygon = as[Polygon]

  def polygon(name: String) = cast[Polygon](name)

  def asCircle = as[Circle]

  def circle(name: String) = cast[Circle](name)


}

final class TableGeoSupport[T <: Document](val table: Table[T]) extends AnyVal {
  def getIntersecting(geo: GeometryType) = GetIntersecting(table, geo, None)


  def getNearest(point: Point) = GetNearest(table, point)
}

trait GeometryApi {


  def circle(point: Point, radius: Double,
             numVertices: Option[Int] = None,
             geoSystem: Option[GeoSystem] = None,
             unit: Option[GeoUnit] = None,
             fill: Option[Boolean] = None): Circle = Circle(point, radius, numVertices, geoSystem, unit, fill)

  def distance[T <: GeometryType](start: Geometry[T], end: GeometryType,
                                  geoSystem: Option[GeoSystem] = None, unit: Option[GeoUnit] = None) = Distance(start, end, geoSystem, unit)


  def point(long: Double, lat: Double) = Point(long, lat)

  def line(points: Point*) = Line(points)

  def geoJson(value: Map[String, Any]) = GeoJson(value)
}


trait GeometryImplicits {
  implicit def tuple2ToPoint(p: (Double, Double)): Point = Point(p._1, p._2)

  implicit def toLineSupport(line: Line) = new LineSupport(line)

  implicit def anyToGeoSupport(any: CastTo) = new AnyGeoSupport(any)

  implicit def seqOfGeometry[T <: GeometryType](seq: ProduceSequence[T]) = new SequenceGeoSupport(seq)

  implicit def tableToGeometry[T <: Document](table: Table[T]) = new TableGeoSupport[T](table)

  implicit def toPolygonSupport(target: ProduceGeometry[Polygon]) = new PolygonSupport(target)


}
