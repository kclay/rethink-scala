package com.rethinkscala

import org.scalatest.FunSuite
import com.rethinkscala.ast.{Var, Expr}
import Blocking._


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
  test("distinct"){

    val seq = Seq(1,2,2,2,43,4,5,5,6,6,6,7,7,1,1,1)

    assert(Expr(seq).distinct,{
      v:Seq[Int]=> v == seq.distinct.sorted
    })

  }

  test("groupedMapReduce"){

    val records = Seq(Map("w"->1,"s"->1,"n"->"a"),Map("w"->2,"s"->2,"n"->"b"),Map("w"->3,"s"->3,"n"->"c"))


    PartialFunction
    val results  = Expr(records).groupedMapReduce(
      x=> x("w"),
      x=> x.pluck("n","s"),
      (a,b)=> r.branch(a("s")< b("s"),b,a)
      ,Map("n"->"none","s"->0)
    )

    assert(results,{
      a:Seq[GroupMapReduceResult]=> a.head("reduction","n").as[String].get == "a" && a.head("reduction","s").as[Int].get == 1  &&
        a(1)("reduction","n").as[String].get == "b" && a(1)("reduction","s").as[Int].get == 2
    })


  }

  test("contains"){

    assert(Expr(1 to 10 by 1) contains(5))

    assert(Expr(1 to 10 by 1) contains((x:Var)=> x > 5))
  }

}
