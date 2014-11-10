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


trait ProduceGeometry[T <: GeometryType] extends Produce[T] with Produce0[T]


case class Distance(start: GeometryType, end: GeometryType, geoSystem: Option[GeoSystem] = None, unit: Option[GeoUnit] = None)
  extends ProduceNumeric {
  override def termType = TermType.DISTANCE

  override lazy val args = buildArgs(start, end)
  override lazy val optargs = buildOptArgs(Map("geoSystem" -> geoSystem, "unit" -> unit))

  def withGeoSystem(system: GeoSystem) = copy(geoSystem = Some(system))

  def withUnit(unit: GeoUnit) = copy(unit = Some(unit))

}

case class Fill(line:Line) extends ProduceGeometry[Polygon]{



  override def termType = TermType.FILL
}


case class Includes[T<:GeometryType](target:ProduceGeometry[T],other:GeometryType) extends ProduceGeometry[T]{

  override def termType = TermType.INCLUDES
}

case class GeoJson(json:String) extends ProduceGeometry[UnknownGeometry]{
  override def termType = TermType.GEOJSON
}

case class GetIntersecting[T<:Document](target:Table[T],geo:GeometryType,index:Option[String]) extends ProduceStreamSelection[T]{

  override lazy val args= buildArgs(target,geo)


  override lazy val optargs=buildOptArgs(Map("index"->index))

  override def termType = TermType.GET_INTERSECTING
}

case class GetNearest[T<:Document](target:Table[T],point:Point,
                                   index:Option[String]=None,maxResults:Option[Int]=None,
                                    maxDistance:Option[Int]=None,unit:Option[GeoUnit] = None,
                                    geoSystem:Option[GeoSystem]=None) extends ProduceArray[T]{



  override lazy val args= buildArgs(target,point)
  override lazy val optargs=buildOptArgs(Map("index"->index,"max_results"->maxResults,
  "max_dist"->maxDistance,"unit"->unit,"geo_system"->geoSystem))

  def withIndex(index:String) = copy(index = Some(index))
  def withMaxResults(amount:Int) = copy(maxResults = Some(amount))
  def withMaxDistance(distance:Int) = copy(maxDistance  = Some(distance))
  def withUnit(unit:GeoUnit) = copy(unit = Some(unit))
  def withGeoSystem(geoSystem:GeoSystem) = copy(geoSystem = Some(geoSystem))
  override def termType = TermType.GET_NEAREST
}
case class ToGeoJson(target:GeometryType) extends ProduceString{
  override def termType = TermType.TO_GEOJSON
}
case class Circle(longLat: Point, radius: Double, numVertices: Option[Int] = None,
                  geoSystem: Option[GeoSystem] = None, unit: Option[GeoUnit] = None,
                  fill: Option[Boolean] = None)
  extends ProduceGeometry[Polygon] with GeometryType{


  override lazy val args = buildArgs(longLat, radius)
  override lazy val optargs = buildOptArgs(Map("numVertices" -> numVertices, "geoSystem" -> geoSystem, "unit" -> unit, "fill" -> fill))

  override def termType = TermType.CIRCLE

  def withVertices(amount: Int) = copy(numVertices = Some(amount))

  def withGeoSystem(system: GeoSystem) = copy(geoSystem = Some(system))

  def withUnit(unit: GeoUnit) = copy(unit = Some(unit))

  def toMeter = copy(unit = Some(Meter))

  def toKiloMeter = copy(unit = Some(KiloMeter))

  def toInternationalMile = copy(unit = Some(InternationalMile))

  def toNauticalMile = copy(unit = Some(NauticalMile))

  def toInternationalFoot = copy(unit = Some(InternationalFoot))

  def withFill = copy(fill = Option(true))

  def withoutFill = copy(fill = Option(false))
}