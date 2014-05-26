package com.rethinkscala

import org.scalatest.FunSuite

import Blocking._
import com.rethinkscala.net.RethinkError

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 4/6/14
 * Time: 4:37 PM
 *
 */


class ManipulateTest extends FunSuite with WithBase{

  test("pluck"){


     assert(testSeq.pluck("player","points"),{
       a:Seq[Map[String,Any]]=> a.head == Map("player"->"Bob","points"->15)
     })


  }

  test("without"){


    assert(testSeq.without("player","points"),{
      a:Seq[Map[String,Any]]=> a.head == Map("id"->2,"type"->"ranked")
    })


  }

  test("merge"){



    val f = testSeq(0) merge testSeq(1)


   assert(f.mapTo[Player].run,{
      p:Player=> p.id == 5
    })

  }

  test("append"){

    val res =(1 to 10) append 11


    assert(res.run,{
      a:Seq[Int]=> a == (1 to 11)
    })
  }

  test("prepend"){
    assert((1 to 10) prepend 0 run,{
      a:Seq[Int] => a == (0 to 10)
    })
  }

  test("difference"){
    var a = 1 to 10
    val b = 5 to 15
    val d = a diff b

    assert(Expr(a) diff b run,{
      c:Seq[Int]=> c == d
    })
  }

  test("setInsert"){

    val a = 1 to 10

    assert(a setInsert 5,{
      b:Seq[Int]=> b == a
    })
    assert(a setInsert 11,{
      b:Seq[Int]=> b == (1 to 11)
    })
  }

  test("setUnion"){
    val a = 1 to 10
    assert(a setUnion a,{
      b:Seq[Int]=> b == a
    })
  }
  test("setIntersection") {

  }

}
