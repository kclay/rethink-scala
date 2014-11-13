package com.rethinkscala.ast

import com.rethinkscala._

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 11/3/2014
 * Time: 11:09 AM 
 */

import com.rethinkscala.{Term, AssocPair}
import ql2.Ql2.Term.TermType


trait ProducePolygon extends ProduceGeometry[Polygon]

trait ProduceLine extends ProduceGeometry[Line]

trait ProducePoint extends ProduceGeometry[Point]

trait ProduceCircle extends ProduceGeometry[Circle]


trait Geometry[T <: GeometryType] extends Typed {
  override val underlying = this

  def toGeoJson = ToGeoJson(underlying)

  def includes(geo: GeometryType) = Includes(this, geo)

  def intersects(geo: GeometryType) = Intersects(GeometryIntersect(this), geo)

  def distance(other: GeometryType) = Distance(this, other)

  def fill = Fill(this)

}

trait ProduceGeometry[T <: GeometryType] extends Geometry[T] with Produce[T] with Produce0[T]

case class Distance[T <: GeometryType](start: Geometry[T], end: GeometryType, geoSystem: Option[GeoSystem] = None, unit: Option[GeoUnit] = None)
  extends ProduceNumeric {
  override def termType = TermType.DISTANCE

  override lazy val args = buildArgs(start, end)
  override lazy val optargs = buildOptArgs(Map("geoSystem" -> geoSystem, "unit" -> unit))

  def withGeoSystem(system: GeoSystem) = copy(geoSystem = Some(system))

  def withUnit(unit: GeoUnit) = copy(unit = Some(unit))

}

case class Fill[T <: GeometryType](line: Geometry[T]) extends ProducePolygon {


  override def termType = TermType.FILL
}


case class Includes[T <: GeometryType](target: Geometry[T], other: GeometryType) extends ProduceGeometry[T] {

  override def termType = TermType.INCLUDES
}

case class Intersects(target: Intersect, geo: GeometryType) extends ProduceBinary {
  override def termType = TermType.INTERSECTS
}

trait Intersect extends WrappedTerm

case class SequenceIntersect[T <: GeometryType](seq: ProduceSequence[T]) extends Intersect {
  override def unwrap = seq
}

case class GeometryIntersect[T <: GeometryType](geo: Geometry[T]) extends Intersect {
  override def unwrap = geo.term
}

case class GeoJson(target: Map[String, Any]) extends ProduceGeometry[UnknownGeometry] {
  override def termType = TermType.GEOJSON
}

case class GetIntersecting[T <: Document](target: Table[T], geo: GeometryType, index: Option[String])
  extends ProduceStreamSelection[T] {

  override lazy val args = buildArgs(target, geo)


  override lazy val optargs = buildOptArgs(Map("index" -> index))

  override def termType = TermType.GET_INTERSECTING
}

case class GetNearest[T <: Document](target: Table[T], point: Point,
                                     index: Option[String] = None, maxResults: Option[Int] = None,
                                     maxDistance: Option[Int] = None, unit: Option[GeoUnit] = None,
                                     geoSystem: Option[GeoSystem] = None) extends ProduceArray[T] {


  override lazy val args = buildArgs(target, point)
  override lazy val optargs = buildOptArgs(Map("index" -> index, "max_results" -> maxResults,
    "max_dist" -> maxDistance, "unit" -> unit, "geo_system" -> geoSystem))

  def withIndex(index: String) = copy(index = Some(index))

  def withMaxResults(amount: Int) = copy(maxResults = Some(amount))

  def withMaxDistance(distance: Int) = copy(maxDistance = Some(distance))

  def withUnit(unit: GeoUnit) = copy(unit = Some(unit))

  def withGeoSystem(geoSystem: GeoSystem) = copy(geoSystem = Some(geoSystem))

  override def termType = TermType.GET_NEAREST
}

case class ToGeoJson[T <: GeometryType](target: Geometry[T]) extends ProduceString {
  override def termType = TermType.TO_GEOJSON
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

case class PolygonSub(target: ProduceGeometry[Polygon], other: Polygon) extends ProducePolygon {
  override def termType = TermType.POLYGON_SUB
}