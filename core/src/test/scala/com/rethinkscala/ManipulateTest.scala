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
      p:Player=> p.id == 2
    })

  }

}
