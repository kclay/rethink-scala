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





  def distance(other: GeometryType) = Distance(this, other)

  def fill = Fill(this)

}

trait ProduceGeometry[T <: GeometryType] extends Geometry[T] with Produce[T] with Produce0[T]{
  override val underlying=this
  def includes(geo: GeometryType) = Includes(underlying, geo)
  def intersects(geo:GeometryType) = Intersects(underlying,geo)
}

trait ProduceGeometryArray[T<:GeometryType] extends ProduceArray[T] with Geometry[T]{

  override val underlying=this

  def includes(geo: GeometryType) = Includes(underlying, geo)
  def intersects(geo:GeometryType) = Intersects(underlying,geo)
}

abstract  class ForwardToGeometry[T](term:Term) extends ForwardTyped(term)

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


class Includes(target:Typed,other:GeometryType) extends Typed{
  def termType = TermType.INCLUDES
}
case class IncludesGeo[T <: GeometryType]( target: Geometry[T],  other: GeometryType) extends
Includes(target,other) with  ProduceBinary

case class IncludesSeq[T]( target:ProduceSequenceLike[T], other:GeometryType) extends Includes(target,other) with ProduceSequence[T]

object Includes{

  def apply[T<:GeometryType](target:ProduceGeometry[T],other:GeometryType) = IncludesGeo(target,other)
  def apply[T](target:ProduceSequenceLike[T],other:GeometryType) = IncludesSeq(target,other)
}



class Intersects(target: Typed, geo: GeometryType) extends Typed {
   def termType = TermType.INTERSECTS
}
object Intersects{

  def apply[T<:GeometryType](target:ProduceGeometry[T],other:GeometryType) = IntersectsGeo(target,other)
  def apply[T](target:ProduceSequenceLike[T],other:GeometryType) = IntersectsSeq(target,other)
}

case class IntersectsGeo[T <: GeometryType]( target: Geometry[T],  other: GeometryType) extends
Intersects(target,other) with  ProduceBinary

case class IntersectsSeq[T]( target:ProduceSequenceLike[T], other:GeometryType) extends Intersects(target,other) with ProduceSequence[T]


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

case class PolygonSub(target: ProduceGeometry[Polygon], other: Polygon) extends ProduceGeometryArray[Polygon] {
  override def termType = TermType.POLYGON_SUB
}