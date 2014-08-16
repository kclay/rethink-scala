package com.rethinkscala

import Blocking._
import org.scalatest.FunSuite


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 10/28/13
 * Time: 10:19 AM 
 */
class SequenceTest extends FunSuite with WithBase {


  test("mapping from ProduceAny with casting to String") {


    val rows = Expr(Seq(Map("hello" -> "1"), Map("hello" -> "2"), Map("hello" -> "3")))

    val a = Expr(Seq(1, 2, 3, 4, 5))



    a.reduce{
      (a,b)=> {
        val c = a + b
        c
      }
    }.ast

    assert(a.reduce(_ + _).run, {
      b: Double=> b == 15
    })



    assert(a.map(x => x * 2), {
      b: Seq[Int] => b == a.array.map(_ * 2).toSeq
    })




    val b = Expr(Seq("1", "2", "3"))



    assert(b.map(x => x add "s"), {
      c: Seq[String] => c == Seq("1s", "2s", "3s")
    })
  }

  override def useVersion = version3
}
