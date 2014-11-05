package com.rethinkscala

import org.scalatest.FunSuite
import Blocking._

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 10/12/14
 * Time: 9:45 AM
 *
 */
case class NestedAddress(labels: List[String] = List("sample")) extends Document

case class NestedUser(name: String, active: Boolean = true, address: NestedAddress = NestedAddress(), id: Option[String] = None) extends Document

class NestedTest extends FunSuite with WithBase {


  test("nested documents") {

    val term = r.table("foo").insert(NestedUser("foo"))
    val query = version3.toQuery(term, 1, None, Map.empty)
    val json = query.json
    println(json)
    assert(term.run, {
      d: InsertResult => d.inserted == 1
    })
  }

  // override def setupDB = false
}
