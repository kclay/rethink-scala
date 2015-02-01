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
case class NestedZip(zip:String) extends Document
case class NestedStreet(zip:NestedZip) extends Document
case class NestedAddress(street:NestedStreet) extends Document

case class NestedUser(name: String, active: Boolean , address: NestedAddress, id: Option[String] = None) extends Document

class NestedTest extends FunSuite with WithBase {


  test("nested documents") {

    val user = NestedUser("foo",true,NestedAddress(NestedStreet(NestedZip("hello"))))
    val foos= r.tableAs[NestedUser]("foo")
    val term = foos.insert(user)
    val query = version3.toQuery(term, 1, None, Map.empty)
    val json = query.json
    println(json)
   val answer =for {
      res <- term.toOpt
      user2 <- foos.get(res.generatedKeys.head).toOpt
    } yield user2

    println(answer)
  }

  // override def setupDB = false
}
