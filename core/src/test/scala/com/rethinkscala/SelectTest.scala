package com.rethinkscala

import org.scalatest.FunSuite


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/2/13
 * Time: 11:46 AM
 *
 */


case class SelectFoo(id: Int) extends Document

case class LargeFoo(token:String,created:Long,email:String,firstName:String,lastName:String,mobile:String,id:Option[String]=None)

class SelectTest extends FunSuite with WithBase {

  import connection.delegate._



  test("select between") {

    val records = for (i <- 1 to 50) yield SelectFoo(i)
    foos.insert(records).run










    assert(foos.between(10, 20).orderBy("id"), {
      f: Seq[SelectFoo] => f.size == 10 & f(0).id == 10 & f.last.id == 19
    })

    assert(foos.between(10, 20, BetweenOptions(leftBound = Some(Bound.Closed), rightBound = Some(Bound.Closed))).orderBy("id"), {
      f: Seq[SelectFoo] => f.size == 11 & f.last.id == 20
    })

    assert(foos.between(10, 20, BetweenOptions(leftBound = Some(Bound.Open), rightBound = Some(Bound.Closed))).orderBy("id"), {
      f: Seq[SelectFoo] => f.size == 10 & f.last.id == 20 && f(0).id == 11
    })


  }

  test("table select") {
    assert(foos, {
      f: Seq[SelectFoo] => f.size == 50
    })
  }
  test("table select ordered") {

    val results = foos.orderBy("id")
    assert(results, {
      f: Seq[SelectFoo] => f.size == 50 && f.last.id == 50
    })
  }

  test("foos.get") {


    assert(foos.get(1), {
      f: SelectFoo => f.id == 1
    })
  }
  test("get_all") {
    assert(foos.getAll( 1, 2).withIndex("id"), {
      a: Seq[SelectFoo] => a.size == 2
    })
  }

  test("select filter") {


    var results = foos.filter(Map("id" -> 1))
    assert(results, {
      f: Seq[SelectFoo] => f.size == 1
    })

    results = foos.filter(f => f \ "id" > 10)
    assert(results, {
      s: Seq[SelectFoo] => s.size == 40
    })

    results = foos.filter(f => f.hasFields("id"))
    assert(results, {
      s: Seq[SelectFoo] => s.size == 50
    })


  }


  class ListHelper[T](ls: List[T]) {
    /** @param size  The size of each sub list */
    def chunk(size: Int) = List.range(0, ls.size, size).map { i => ls.slice(i, i + size)}
  }

  implicit def list2helper[T](ls: List[T]) = new ListHelper(ls)

  test("5000 results") {


    val token = "KJHGFGRTYJHMNBVHJLKJY^TUYGJHBIOHBnkfhdhtyukhj,bmvgjfyutilhkjbvhgkuihjlkyut678965r783945poi2jhkegwryiuopk;lsjdfnbhvgkuiyiopk;rl/qn,.ejrbdfhgluy9ipojkhjgkiuyoijklbhgkuiojhlguiy89uihjajdfhgkjnsfakljhdi;jakdfsnbljhasjd;fljbasdo;fja;dkhvp’aoskdf;lahsdfkjasdkhjasdgfoiauhsdflhasdfhalisdhufliaksbdfljahdgsfkajbsdlfiuhasdfhailusdhfliasduhflaisdhflias"
    val chunks = List.range(0, 50000).chunk(125)


    def string = randomAlphanumericString(5)
    var large = foos.to[LargeFoo]
    chunks.map {
      chunk =>
        large.insert(chunk.map(id=>LargeFoo(token,id,string,string,string,string))).run
    }

    val length = large.count().run

    var total = 0
    large.orderBy().withIndex("id").toOpt.map {
      case seq =>
        val len = seq.length
        val c = seq.chunks
        seq.foreach{
          c=> total = total+1


        }


    }
    assert(length == Right(total.toDouble))

  }


  lazy val foos = table.to[SelectFoo]
}
        


