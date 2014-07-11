package com.rethinkscala


import org.scalatest.FunSuite
import com.rethinkscala.ast._
import com.rethinkscala.net.Blocking._

case class Transform(a:Int,b:Int,id:Option[String]=None) extends Document
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

  test("concat_map"){
    val a = Map("a"->Seq(1,2,3,4,5,6))
    val e = Expr(Seq(a,a))

    assert( e.concatMap(v=> v.seq[Int]("a")),{
      s:Seq[Int]=> s.size == 12
    })

  }
  test("orderBy"){


  table.asInstanceOf[Table[Transform]]
    table.insert(Seq(Transform(1,1),Transform(2,2),Transform(3,3))) run
    val index = table.indexCreate("a")

    index.run
    assert(table.to[Transform].orderBy("a"),{
      s:Seq[Transform]=>  s(0).a == 1 && s(1).a ==2 && s(2).a == 3
    })
    assert(table.to[Transform].orderBy("a","a"),{
      s:Seq[Transform]=>  s(0).a == 1 && s(1).a ==2 && s(2).a == 3
    })
    assert(table.to[Transform].orderByIndex("a".desc),{
      s:Seq[Transform]=>  s(0).a == 3 && s(1).a ==2 && s(2).a == 1
    })
    assert(table.to[Transform].orderBy("b".desc),{
      s:Seq[Transform]=>  s(0).a == 3 && s(1).a ==2 && s(2).a == 1
    })



  }

  test("skip"){

    assert(Expr(Seq(1,2,3,4,5,6,7,8,9,10)).skip(5),{
      s:Seq[Int]=> s.size == 5
    })
  }
  test("limit"){
    assert(Expr((1 to 10 by 1)).limit(5),{
      s:Seq[Int]=> s.size == 5
    })
  }
  test("slice"){
    val seq = Expr(1 to 10 by 1)
    assert(seq(5 ~> 9),{
      s:Seq[Int] => s == (1 to 10 by 1).slice(5,9)
    })

  }
  test("nth"){

    assert(Expr(1 to 10 by 1)(4),{
      s:Int => s == 5
    })
  }

  test("indexes_of"){
    assert(Expr(1 to 10 by 1).indexesOf(5) ,{
      s:Seq[Long]=> s == Seq(4)
    })


    assert(Expr(1 to 10 by 1).indexesOf(r.row > 5),{
      s:Seq[Long]=>s == (5 to 9 by 1)
    })
  }
  test("is_empty"){
    assert(Expr(1 to 10 by 1).isEmpty,false)
    assert(Expr(Seq()).isEmpty)
  }
  test("union"){
    assert(Expr(1 to 5 by 1).union(Expr(6 to 10 by 1)),{
      s:Seq[Any]=> s == (1 to 10 by 1)
    })
  }

  test("sample"){
    assert(Expr(1 to 5 by 1).sample(3),{
      s:Seq[Int]=> s.size == 3
    })


  }

}
