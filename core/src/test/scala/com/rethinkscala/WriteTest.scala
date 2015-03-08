package com.rethinkscala

import com.fasterxml.jackson.annotation.JsonProperty
import org.scalatest.FunSuite
import Blocking._


/** Created with IntelliJ IDEA.
  * User: keyston
  * Date: 6/29/13
  * Time: 5:34 PM
  *
  */

case class Foo(id: Option[String] = None, a: Int, b: Int) extends Document {
  override protected def afterInsert(id: String) {
    println(id)
  }
}

case class NullItem(a: Int = 1) extends Document

case class NullTest(id: Option[String] = None, item: NullItem) extends Document

case class NullTest2(id: Option[String] = None, item: Option[NullItem] = None) extends Document

case class Foo2(id: String, a: Int, b: Int, @JsonProperty("is_fav") fav: Boolean) extends Document

class WriteTest extends FunSuite with WithBase {

  implicit def string2Option(s: String) = Some(s)




  test("serialization of insert document") {

    val foo = r.table("foo")
    val nullItem: NullItem = null
    val noNulls = foo.insert(NullTest(None, nullItem))
    val noNullsWithId = foo.insert(NullTest("a", nullItem))
    val noNulls2 = foo.insert(NullTest2())
    val noNullWithId2 = foo.insert(NullTest2("a"))
    val withValue = foo.insert(NullTest2("a", Some(new NullItem(1))))




    assert(toJson(noNulls) == "[1,[56,[[15,[\"foo\"]],{}]],{}]")

    assert(toJson(noNullsWithId) == "[1,[56,[[15,[\"foo\"]],{\"id\":\"a\"}]],{}]")

    assert(toJson(noNulls2) == "[1,[56,[[15,[\"foo\"]],{}]],{}]")
    //    assert(toJson(withNulls2) == "[1,[56,[[15,[\"foo\"]],{\"item\":null}]],{}]")
    assert(toJson(noNullWithId2) == "[1,[56,[[15,[\"foo\"]],{\"id\":\"a\"}]],{}]")
    val b = toJson(withValue)
    assert(toJson(withValue) == "[1,[56,[[15,[\"foo\"]],{\"id\":\"a\",\"item\":{\"a\":1.0}}]],{}]")
  }
  test("insert documents") {


    assert(table.insert(Foo("a", a = 1, b = 1)), {
      rs: InsertResult => rs.inserted == 1
    })

    assert(table.insert(
      Seq(
        Foo("b", 1, 2), Foo("c", 1, 2))
    ), {
      rs: InsertResult => rs.inserted == 2
    })

  }

  test("insert documents with return vals") {

    scala.collection.convert.Wrappers
    val insert = table.insert(Foo(None, 4, 5)).withResults
    assert(insert, {
      i: InsertResult => i.inserted == 1 && i.returnedValue[Foo].isDefined
    })


  }
  test("replace") {

    val fetch2= table.get("foo")

    table.insert(Foo("foo",a=1,b=1)).run
    val query = fetch2.replace(Foo(Some("foo"), 10, 60)).withResults
    val json = toJson(query)
    assert(query, {
      c: ChangeResult => c.replaced == 1 && c.returnedValue[Foo].exists(_.a == 10)
    })

    var replace = fetch2.replace(Map( "id"->"foo","b" -> 29))

    assert(replace, {
      rr: ChangeResult => rr.replaced == 1
    })

    replace = fetch2.replace(v => v.merge(Map("is_fav" -> true)))
    assert(replace, {
      cr: ChangeResult => cr.replaced == 1
    })
    assertAs[Foo2](fetch2, {
      f: Foo2 => f.fav
    })

  }

  test("updating data") {

    val fetch =  table.get("aa")

    table.insert(Foo("aa", a = 1, b = 1)).run

    var update = fetch.update(Map("a" -> 2, "b" -> 2))

    assert(update, {
      u: ChangeResult => u.replaced == 1
    })

    assertAs[Foo](fetch, {
      f: Foo => f.id.get == "aa" && f.a == 2 && f.b == 2
    })


    update = fetch.update(v => Map("a" -> (v \ "a").add(3)))

    assert(update, {
      u: ChangeResult => u.replaced == 1
    })

    assertAs[Foo](fetch, {
      f: Foo => f.a == 5
    })
    update = fetch.update(v => Map("a" -> r.row("a").add(3)))

    assert(update, {
      u: ChangeResult => u.replaced == 1
    })
    assertAs[Foo](fetch, {
      f: Foo => f.a == 8
    })


    update = fetch.update(b => r.branch(b("a") === 8,
      Map("a" -> 10),
      Map("a" -> 0)))

    assert(update,{
      u:ChangeResult=> u.replaced == 1
    })
    assertAs[Foo](fetch,{
      f:Foo=> f.a == 10
    })


  }



  test("delete data") {

    val fetch =  table.get("bb")

    table.insert(Foo("bb", a = 1, b = 1)).run

    assert(fetch.delete, {
      cr: ChangeResult => cr.deleted == 1
    })
  }

}
