package com.rethinkscala

import org.scalatest.FunSuite

import Blocking._
import com.rethinkscala.magnets._
import com.rethinkscala.reflect.Reflector
import com.rethinkscala.magnets.FieldFilterReceptacle


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 3/20/14
 * Time: 3:06 PM
 *
 */

class AggregationTest extends FunSuite with WithBase {

  test("reduce") {
    val seq = 1 to 10 by 1
    assert(Expr(seq).reduce(_ + _), {
      v: Int => seq.reduce(_ + _) == v
    })

    /*assert(Expr(seq).reduce(_ + _, 10), {
      v: Int => seq.foldLeft(10)(_ + _) == v
    })*/
  }

  test("count") {
    val seq = Seq(1, 2, 3, 3, 3, 4, 5, 6, 7)

    assert(Expr(seq).count(3), {
      v: Double => v == 3
    })

    assert(Expr(seq).count(x => x > 3), {
      v: Double => v == 4
    })
    assert(Expr(seq).count, {
      v: Double => v == seq.size
    })


  }
  test("distinct") {

    val seq = Seq(1, 2, 2, 2, 43, 4, 5, 5, 6, 6, 6, 7, 7, 1, 1, 1)

    assert(Expr(seq).distinct, {
      v: Seq[Int] => v == seq.distinct.sorted
    })

  }

  lazy val testSeq={

    val json = """[
                 |    {"id": 2, "player": "Bob", "points": 15, "type": "ranked"},
                 |    {"id": 5, "player": "Alice", "points": 7, "type": "free"},
                 |    {"id": 11, "player": "Bob", "points": 10, "type": "free"},
                 |    {"id": 12, "player": "Alice", "points": 2, "type": "free"}
                 |]""".stripMargin

    Expr(Reflector.fromJson[Seq[Map[String, Any]]](json))

  }

  test("group") {



    val seq = testSeq
    val check = {
      g:GroupResult[String]=> g.records.size == 2 && g.records.head.group == "Alice" && g.records.head.values.head.get("id") == Some(5)
    }
    assert(seq.group("player"),check)


    val res = seq.group({
      v:Var=> v.pluck("player")
    }.as[String],"id")

    def mf[T](v:T)(implicit m:Manifest[T]) = m

    println(mf(res))

    res.run








  }

  test("max"){

    val res = testSeq.max("points").as[Int]("points")

   assert(res,{
    p:Int=> p == 15
   })
  }

  test("contains") {

    assert(Expr(1 to 10 by 1) contains 5)

    assert(Expr(1 to 10 by 1) contains ((x: Var) => x > 5))
  }

}
