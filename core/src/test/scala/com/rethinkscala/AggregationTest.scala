package com.rethinkscala

import com.rethinkscala.Implicits.Blocking._
import com.rethinkscala.net.DefaultCursor
import org.scalatest.FunSuite



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

    assert(Expr(seq).reduce(_ + _).run, {
      v: Double => seq.reduce(_ + _) == v
    })


  }


  test("distinct") {

    val seq = Seq(1, 2, 2, 2, 43, 4, 5, 5, 6, 6, 6, 7, 7, 1, 1, 1)

    assert(Expr(seq).distinct, {
      v: DefaultCursor[Int] => v == seq.distinct.sorted
    })

  }



  test("group") {


    val seq = testSeq
    val check = {
      g:GroupResult[String]=> g.size == 2 && g.head.group == "Alice" && g.head.values.head.get("id") == Some(5)
    }


   val s =Seq(GroupResultRecord[Int](1,Seq(Map.empty)))

    assert(seq.group("player"),check)


   /* val func = {
      v:Var=> v.pluck("player")
    }.as[String]


    val res = seq.group(func,"id")

    def mf[T](v:T)(implicit m:Manifest[T]) = m


    println(mf(res))

    res.run*/








  }

  test("ungroup"){
    val seq = testSeq

    val m = seq.group("player").max("points")


  }

  test("count"){

    assert(testSeq.count().toOpt == Some(4))
    assert(testSeq("points").count(15).toOpt == Some(1))
    assert(testSeq("points").count(p=>p >=10 ).toOpt == Some(2))
    assert(testSeq.count((p:Var)=> p("points") >=10).toOpt == Some(2))
    val seq = Expr(Seq(1, 2, 3, 3, 3, 4, 5, 6, 7))


    assert(seq.count(3), {
      v: Double => v == 3
    })

    assert(seq.count(x => x > 3), {
      v: Double => v == 4
    })
    assert(seq.count, {
      v: Double => v == 9
    })
  }

  test("max"){

    val res = testSeq.max("points").int("points")


   assert(res.run,{
    p:Int=> p == 15

   })

    assert(testSeq.max(x=> x("points")).int("points").toOpt == Some(15))
  }


  test("sum"){
    val seq = 1 to 10 by 1
    val sum = seq.sum

    assert(Expr(seq).sum().toOpt == Some(sum))

    assert(testSeq.sum("points").toOpt == Some(34))

    assert(testSeq.sum(v=> v("id") + v("points")).toOpt ==  Some(64))

  }
  test("avg"){

    assert(Expr(1 to 10 by 1).avg.toOpt == Some(5.5))
    assert( testSeq.avg("points").toOpt == Some(8.5))
    assert(testSeq.avg(v=> v \ "points" + v\"id").toOpt == Some(16))
  }
  test("contains") {

    val a = Expr(1 to 10 by 1)

    assert(Expr(1 to 10 by 1) contains 5)

    assert(Expr(1 to 10 by 1) contains (x=> x > 5))


    //val b = Var(1).seq[Int]("foo")
    //print(b)



     Expr(1 to 10).filter(f=> f("industries").contains(i=> i \ "userId" ==="700"))
  //  Expr(1 to 10).filter(f=> f.anySeq("industries").contains(i=> i \ "userId" ==="700"))
    val res = Expr('a' to 'd').contains("b")
     print(res.ast)
    assert(res)


  }

  override def useVersion = version3
}
