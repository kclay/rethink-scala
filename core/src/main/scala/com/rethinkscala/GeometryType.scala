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


trait GeoCastDelegate[R[_<:GeometryType]] extends Any{

  def cast[T<:GeometryType](name:String):R[T]
  def to[T<:GeometryType]:R[T]
}
final class AnyGeoCastSupport(val target:CastTo) extends AnyVal with GeoCastDelegate[ProduceGeometry]{
  override def cast[T <: GeometryType](name:String) = target.field(name).asInstanceOf[ProduceGeometry[T]]

  override def to[T <: GeometryType] = target.asInstanceOf[ProduceGeometry[T]]
}
final class GetFieldCastSupport(val target:ProduceArray[_]) extends AnyVal with GeoCastDelegate[ProduceGeometryArray]{
  override def cast[R <: GeometryType](name: String) = ???

  override def to[R <: GeometryType] = new ForwardTyped(target) with ProduceGeometryArray[R]
}

final class GeoCastSupport[R[_<:GeometryType]](val delegate:GeoCastDelegate[R]) extends AnyVal {

  private def cast[T <: GeometryType](name: String) = delegate.cast[T](name)

  private def to[T <: GeometryType]=delegate.to[T]

  def toPoint = to[Point]

  def point(name: String) = cast[Point](name)

  def toLine = to[Line]

  def line(name: String) = cast[Line](name)

  def toPolygon = to[Polygon]

  def polygon(name: String) = cast[Polygon](name)

 // def asCircle = as[Circle]

 // def circle(name: String) = cast[Circle](name)
  def unknownGeometry(name:String) = cast[UnknownGeometry](name)
  def toUnknownGeometry = to[UnknownGeometry]
  
  
  


}


case class Circle(longLat: Point, radius: Double, numVertices: Option[Int] = None,
                  geoSystem: Option[GeoSystem] = None, unit: Option[GeoUnit] = None,
                  fillCircle: Option[Boolean] = None)
  extends ProducePolygon with GeometryType {


  override lazy val args = buildArgs(longLat, radius)
  override lazy val optargs = buildOptArgs(Map("numVertices" -> numVertices, "geoSystem" -> geoSystem, "unit" -> unit, "fill" -> fillCircle))

  override def termType = TermType.CIRCLE

  def withVertices(amount: Int) = copy(numVertices = Some(amount))

  def withGeoSystem(system: GeoSystem) = copy(geoSystem = Some(system))

  def withUnit(unit: GeoUnit) = copy(unit = Some(unit))

  def toMeter = copy(unit = Some(Meter))

  def toKiloMeter = copy(unit = Some(KiloMeter))

  def toInternationalMile = copy(unit = Some(InternationalMile))

  def toNauticalMile = copy(unit = Some(NauticalMile))

  def toInternationalFoot = copy(unit = Some(InternationalFoot))

  def withFill = copy(fillCircle = Option(true))

  def withoutFill = copy(fillCircle = Option(false))
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

  def line(points:Seq[Point])  = Line(points)
  def line(point:Point,points: Point*) = Line(points.+:(point))

  def geoJson(value: Map[String, Any]) = GeoJson(value)
}


trait GeometryImplicits {
  implicit def tuple2ToPoint(p: (Double, Double)): Point = Point(p._1, p._2)

  implicit def toLineSupport(line: Line) = new LineSupport(line)

  implicit def anyToGeoSupport(any: CastTo) = new GeoCastSupport[ProduceGeometry](new AnyGeoCastSupport(any))
  implicit def toGetFieldCastSupport[T](target:ProduceArray[T])= new GeoCastSupport[ProduceGeometryArray](new GetFieldCastSupport(target))




 // implicit def toGetFieldCastSupport[T](target:ProduceArray[T]) = new GetFieldCastSupport(target)
  //implicit def toGeoCastSupport[R[_]](implicit delegate:GeoCastDelegate[R]) = new GeoCastSupport[R](delegate)


  //implicit def toGeoCastSupport(delegate:GeoCastDelegate) = new GeoCastSupport(delegate)

  implicit def tableToGeometry[T <: Document](table: Table[T]) = new TableGeoSupport[T](table)

  implicit def toPolygonSupport(target: ProduceGeometry[Polygon]) = new PolygonSupport(target)


}
