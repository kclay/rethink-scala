package com.rethinkscala

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}
import org.scalatest.FunSuite
import Blocking._

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 10/12/14
 * Time: 9:45 AM
 *
 */
case class NestedZip(zip:String)
case class NestedStreet(zip:NestedZip) extends Document


@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, include = JsonTypeInfo.As.PROPERTY)
trait NestedAddress

case class NestedAddress1(street:NestedStreet)       extends NestedAddress

case class NestedAddress2(name:String)  extends NestedAddress

case class NestedUser(name: String, active: Boolean , address:List[NestedAddress], id: Option[String] = None)

class NestedTest extends FunSuite with WithBase {


  test("nested documents") {

    val user = NestedUser("foo",true,List(NestedAddress1(NestedStreet(NestedZip("hello"))),NestedAddress2("foo")))
    val foos= r.tableAs[NestedUser]("foo")
    val term = foos.insert(user)
    val query = version3.toQuery(term, 1, None, Map.empty)
    val json = query.json

   val answer =for {
      res <- term.toOpt
      user2 <- foos.get(res.generatedKeys.head).toOpt
    } yield user2

    println(answer)
  }

  // override def setupDB = false
}
