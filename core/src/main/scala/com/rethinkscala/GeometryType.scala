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


trait GeometryType{

  def toGeoJson = ToGeoJson(this)

  def includes(geo:GeometryType) = Includes(this,geo)

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

case class Point(long: Double, lat: Double) extends GeometryType with Term {


  override lazy val args = buildArgs(long, lat)

  override def termType = TermType.POINT


  def this(l: List[Double]) {
    this(l(0), l(1))
  }
}


case class Polygon(points: Seq[Point]) extends GeometryType with Term {


  override def termType = TermType.POLYGON


}

case class Line(points:Seq[Point])  extends GeometryType with Term{
  override def termType = TermType.LINE
}


sealed class LineSupport(line:Line) extends AnyVal{
  def fill = Fill(line)
}

class UnknownGeometry(geo:GeometryType) extends GeometryType{
  def isPoint = geo.isInstanceOf[Point]
  def isPolygon = geo.isInstanceOf[Polygon]
  def isLine = geo.isInstanceOf[Line]
  def isCircle = geo.isInstanceOf[Circle]
  def circle = if(isCircle) Some(geo.asInstanceOf[Circle]) else None
  def polygon = if(isPolygon) Some(geo.asInstanceOf[Polygon]) else None
  def line = if(isLine) Some(geo.asInstanceOf[Line]) else None
  def point = if(isPoint ) Some(geo.asInstanceOf[Point]) else None
  //def map(pf:PartialFunction)






}
trait GeometryApi {
  



  def circle(point: Point, radius: Double,
             numVertices: Option[Int] = None,
             geoSystem: Option[GeoSystem] = None,
             unit: Option[GeoUnit] = None,
             fill: Option[Boolean] = None): Circle = Circle(point, radius, numVertices, geoSystem, unit, fill)

  def distance(start: GeometryType, end: GeometryType,
               geoSystem: Option[GeoSystem] = None, unit: Option[GeoUnit] = None) = Distance(start, end, geoSystem, unit)


  def point(long: Double, lat: Double) = Point(long, lat)
}


trait GeometryImplicits {
  implicit def tuple2ToPoint(p: (Double, Double)): Point = Point(p._1, p._2)

  implicit def toLineSupport(line:Line) = new LineSupport(line)


}
