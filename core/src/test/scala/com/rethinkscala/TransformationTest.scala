package com.rethinkscala


import org.scalatest.FunSuite
import com.rethinkscala.ast._
import Blocking._

class TransformationTest extends FunSuite with WithBase {

  test("test map") {


      //https://issues.scala-lang.org/browse/SI-6221
    //https://github.com/scala/scala/pull/2650

    //val term = r.table[Document]("marvel").map(hero =>Expr(Seq(hero.as[Int]("a"),hero.as[Int]("b"))))
    //r.table[Document]("marvel").map(hero=> Expr(Seq(1,2)))

    val a = Expr(Seq(1, 2, 3, 4, 5))

    assert(a.map(x => x * 2), {
      b: Seq[Int] => b == a.array.map(_ * 2)
    })

    val b = Expr(Seq("1", "2", "3"))



    assert(b.map(x => x add "s"), {
      c: Seq[String] => c == Seq("1s", "2s", "3s")
    })

  }

  test("with_fields"){
    val a= Expr(Seq(Map("a"->1,"b"->Map("a"->1,"b"->2))))
      type M = Seq[Map[String,Any]]
    assert(a.withFields("a"),{
     d:M=> d(0).get("a") == Some(1)
    })

    assert(a.withFields(Map("b"->Map("a"->true))),{
      d:M=>DocPath(d(0),List("b","a")).as[Int] == Some(1)
    })

    assert(a.withFields(Map("b"->"a")),{
      d:M=>DocPath(d(0),List("b","a")).as[Int] == Some(1)
    })



  }
}
