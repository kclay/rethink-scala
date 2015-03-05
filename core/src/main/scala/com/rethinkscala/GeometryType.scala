package com.rethinkscala

import com.rethinkscala.ast._
import ql2.Ql2.Term.TermType

import scala.collection.generic.SeqForwarder

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 11/3/2014
 * Time: 12:10 PM 
 */


trait GeometryType

trait GeoSystem extends WrappedValue[String]

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

  override def termType:TermType = TermType.POINT

  def this(l: List[Double]) {
    this(l(0), l(1))
  }
}


class Polygon(val points: Seq[Point]) extends GeometryType with ProducePolygon {

  override lazy val args = buildArgs(points: _*)

  override def termType:TermType = TermType.POLYGON


}

object Polygon {

  def apply(points: Seq[Point]):Polygon = new Polygon(points)
}


case class Line(points: Seq[Point]) extends GeometryType with ProduceLine {

  override lazy val args = buildArgs(points: _*)

  override def termType: TermType = TermType.LINE
}


final class LineSupport(val line: Line) extends AnyVal {
  def fill: Fill[Line] = Fill(line)
}


final class PolygonSupport(val polygon: ProduceGeometry[Polygon]) extends AnyVal {
  def polygonSub(other: Polygon): PolygonSub = PolygonSub(polygon, other)
}

case class PolygonSubResults(polygons: Seq[Polygon]) extends GeometryType with SeqForwarder[Polygon] {
  protected override def underlying = polygons
}

class UnknownGeometry(geo: GeometryType) extends GeometryType {
  def isPoint: Boolean = geo.isInstanceOf[Point]

  def isPolygon: Boolean = geo.isInstanceOf[Polygon]

  def isLine: Boolean = geo.isInstanceOf[Line]

  def isCircle: Boolean = geo.isInstanceOf[Circle]

  def circle: Option[Circle] = if (isCircle) Some(geo.asInstanceOf[Circle]) else None

  def polygon: Option[Polygon] = if (isPolygon) Some(geo.asInstanceOf[Polygon]) else None

  def line: Option[Line] = if (isLine) Some(geo.asInstanceOf[Line]) else None

  def point: Option[Point] = if (isPoint) Some(geo.asInstanceOf[Point]) else None


}


trait GeoCastDelegate[R[_ <: GeometryType]] extends Any {

  def cast[T <: GeometryType](name: String): R[T]

  def to[T <: GeometryType]: R[T]
}

final class AnyGeoCastSupport(val target: CastTo) extends AnyVal with GeoCastDelegate[ProduceGeometry] {
  override def cast[T <: GeometryType](name: String) = target.field(name).asInstanceOf[ProduceGeometry[T]]

  override def to[T <: GeometryType] = target.asInstanceOf[ProduceGeometry[T]]
}

final class GetFieldCastSupport(val target: ProduceArray[_]) extends AnyVal with GeoCastDelegate[ProduceGeometryArray] {
  override def cast[R <: GeometryType](name: String) = ???

  override def to[R <: GeometryType] = new ForwardTyped(target) with ProduceGeometryArray[R]
}

final class GeoCastSupport[R[_ <: GeometryType]](val delegate: GeoCastDelegate[R]) extends AnyVal {

  private def cast[T <: GeometryType](name: String): R[T] = delegate.cast[T](name)

  private def to[T <: GeometryType]: R[T] = delegate.to[T]

  def toPoint: R[Point] = to[Point]

  def point(name: String): R[Point] = cast[Point](name)

  def toLine: R[Line] = to[Line]

  def line(name: String): R[Line] = cast[Line](name)

  def toPolygon: R[Polygon] = to[Polygon]

  def polygon(name: String): R[Polygon] = cast[Polygon](name)

  def unknownGeometry(name: String): R[UnknownGeometry] = cast[UnknownGeometry](name)

  def toUnknownGeometry: R[UnknownGeometry] = to[UnknownGeometry]


}


case class Circle(longLat: Point, radius: Double, numVertices: Option[Int] = None,
                  geoSystem: Option[GeoSystem] = None, unit: Option[GeoUnit] = None,
                  fillCircle: Option[Boolean] = None)
  extends ProducePolygon with GeometryType {


  override lazy val args = buildArgs(longLat, radius)
  override lazy val optargs = buildOptArgs(Map("numVertices" -> numVertices, "geoSystem" -> geoSystem, "unit" -> unit, "fill" -> fillCircle))

  override def termType: TermType = TermType.CIRCLE

  def withVertices(amount: Int): Circle = copy(numVertices = Some(amount))

  def withGeoSystem(system: GeoSystem): Circle = copy(geoSystem = Some(system))

  def withUnit(unit: GeoUnit): Circle = copy(unit = Some(unit))

  def toMeter: Circle = copy(unit = Some(Meter))

  def toKiloMeter: Circle = copy(unit = Some(KiloMeter))

  def toInternationalMile: Circle = copy(unit = Some(InternationalMile))

  def toNauticalMile: Circle = copy(unit = Some(NauticalMile))

  def toInternationalFoot: Circle = copy(unit = Some(InternationalFoot))

  def withFill: Circle = copy(fillCircle = Option(true))

  def withoutFill: Circle = copy(fillCircle = Option(false))
}


final class TableGeoSupport[T <: AnyRef](val table: Table[T]) extends AnyVal {

  def getIntersecting(geo: GeometryType): GetIntersecting[T] = GetIntersecting(table, geo, None)

  def getNearest(point: Point): GetNearest[T] = GetNearest(table, point)
}

trait GeometryApi {


  def circle(point: Point, radius: Double,
             numVertices: Option[Int] = None,
             geoSystem: Option[GeoSystem] = None,
             unit: Option[GeoUnit] = None,
             fill: Option[Boolean] = None): Circle = Circle(point, radius, numVertices, geoSystem, unit, fill)

  def distance[T <: GeometryType](start: Geometry[T], end: GeometryType, geoSystem: Option[GeoSystem] = None,
                                  unit: Option[GeoUnit] = None): Distance[T] = Distance(start, end, geoSystem, unit)

  def point(long: Double, lat: Double): Point = Point(long, lat)

  def polygon(points: Seq[Point]): Polygon = Polygon(points)

  def polygon(point: Point, points: Point*): Polygon = Polygon(points.+:(point))

  def line(points: Seq[Point]): Line = Line(points)

  def line(point: Point, points: Point*) = Line(points.+:(point))

  def geoJson(value: Map[String, Any]): GeoJson = GeoJson(value)
}


trait GeometryImplicits {
  implicit def tuple2ToPoint(p: (Double, Double)): Point = Point(p._1, p._2)

  implicit def tuple2ToPointInt(p: (Int, Int)): Point = Point(p._1.toDouble, p._2.toDouble)

  implicit def tuple2ToPointInt2(p: (Int, Double)): Point = Point(p._1.toDouble, p._2)

  implicit def tuple2TopPointInt3(p: (Double, Int)): Point = Point(p._1, p._2.toDouble)

  implicit def toLineSupport(line: Line): LineSupport = new LineSupport(line)

  implicit def anyToGeoSupport(any: CastTo) = new GeoCastSupport[ProduceGeometry](new AnyGeoCastSupport(any))

  implicit def toGetFieldCastSupport[T](target: ProduceArray[T]): GeoCastSupport[ProduceGeometryArray] = new GeoCastSupport[ProduceGeometryArray](new GetFieldCastSupport(target))

  implicit def tableToGeometry[T <: AnyRef](table: Table[T]): TableGeoSupport[T] = new TableGeoSupport[T](table)

  implicit def toPolygonSupport(target: ProduceGeometry[Polygon]): PolygonSupport = new PolygonSupport(target)


}
