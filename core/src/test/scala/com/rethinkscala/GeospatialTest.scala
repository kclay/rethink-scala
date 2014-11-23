package com.rethinkscala

import Blocking._

import com.rethinkscala.ast.{ProduceArray, ProduceAny}
import org.scalatest.FunSuite

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 11/3/2014
 * Time: 11:36 AM 
 */
class GeospatialTest extends FunSuite with WithBase {

  val circlePolygon = Polygon(Seq(Point(-122.423246, 37.770378359589635),
    Point(-122.42546028297909, 37.770551456627935), Point(-122.42758950229496, 37.771064098057934),
    Point(-122.42955185866998, 37.77189659001296), Point(-122.43127195684315, 37.77301695025619),
    Point(-122.43268370038074, 37.77438213572423), Point(-122.43373283081442, 37.775939694859375),
    Point(-122.43437901366829, 37.77762978150455), Point(-122.43459739107298, 37.779387453240034),
    Point(-122.43437954090827, 37.78114516606947), Point(-122.43373380502685, 37.78283536974032),
    Point(-122.43268497325063, 37.78439310401677), Point(-122.4312733345876, 37.78575849607806),
    Point(-122.42955313153989, 37.78687906291455), Point(-122.42759047650742, 37.787711730010884),
    Point(-122.42546081021908, 37.78822448846657), Point(-122.423246, 37.78839762659882),
    Point(-122.42103118978093, 37.78822448846657), Point(-122.4189015234926, 37.787711730010884),
    Point(-122.41693886846012, 37.78687906291455), Point(-122.41521866541241, 37.78575849607806),
    Point(-122.41380702674938, 37.78439310401677), Point(-122.41275819497316, 37.78283536974032),
    Point(-122.41211245909174, 37.78114516606947), Point(-122.41189460892703, 37.779387453240034),
    Point(-122.41211298633172, 37.77762978150455), Point(-122.41275916918559, 37.775939694859375),
    Point(-122.41380829961928, 37.77438213572423), Point(-122.41522004315686, 37.77301695025619),
    Point(-122.41694014133003, 37.77189659001296), Point(-122.41890249770505, 37.771064098057934),
    Point(-122.42103171702092, 37.770551456627935), Point(-122.423246, 37.770378359589635)))

  test("circle") {

    //[1, [165, [[2, [-122.423246, 37.779388]], 1000]], {}]
    //[1, [165, [[2, [-122.423246, 37.779388]], 1000]], {}]
    val term = r.circle((-122.423246, 37.779388), 1000)




    assert(term.toOpt.contains(circlePolygon))
  }
  test("distance") {

    val term = r.distance(r.point(-122.423246, 37.779388), r.point(-117.220406, 32.719464), unit = KiloMeter)
    val json = toJson(term)
    val checks = Seq(734.1252496021841 /* x64 */ , 734.12524960218490833 /* x32 */)
    assert(term.run, checks.contains _)
  }

  test("fill") {
    val check = Polygon(Seq(Point(-122.423246, 37.779388),
      Point(-122.423246, 37.329898),
      Point(-121.88642, 37.329898),
      Point(-121.88642, 37.779388),
      Point(-122.423246, 37.779388)
    ))
    val term = r.line(
      (-122.423246, 37.779388),
      (-122.423246, 37.329898),
      (-121.886420, 37.329898),
      (-121.886420, 37.779388)
    ).fill

    assert(term.run, {
      p: Polygon => p == check
    })
  }

  test("geojson") {
    val term = r.geoJson(Map("type" -> "Point", "coordinates" -> Seq(-122.423246, 37.779388)))
    val point: Point = (-122.42324600000000601, 37.779387999999997305)
    assert(term.run, {
      g: UnknownGeometry => g.point.contains(point)
    })
    val term2 = r.geoJson(Map("type" -> "LineString", "coordinates" -> Seq(Seq(-122.423246, 37.779388), Seq(0, 1))))
    val line: Line = Line(Seq(Point(-122.423246, 37.779388), Point(0, 1)))
    assert(term2.run, {
      g: UnknownGeometry => g.line.contains(line)
    })

  }
  test("includes"){
    val point1 = r.point(-117.220406,32.719464)
    val point2 = r.point(-117.206201,32.725186)





  assert(r.circle(point1, 2000).includes(point2).run,{
    b:Boolean => b
  })


   table.insertMap(Map("includes"->r.circle(point1, 2000))).run




    assert(table("includes").toPolygon.includes(point2).run,{
      s:Seq[Polygon]=>     s.size == 1
    })


  }

  test("intersects"){
    val point1 = r.point(-117.220406,32.719464)
    val point2 = r.point(-117.206201,32.725186)

    val circle = r.circle(point1, 2000)
    assert(circle.intersects(point2).run,{
      b:Boolean=> b
    })

    table.insertMap(Map("intersects"->circle)).run

    assert(table("intersects").toPolygon.intersects(point2).run,{
      s:Seq[Polygon]=>     s.size == 1
    })
  }

}
