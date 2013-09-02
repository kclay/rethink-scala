package com.rethinkscala

import org.scalatest.FunSuite
import com.rethinkscala.reflect.Reflector

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/13/13
 * Time: 12:08 PM
 *
 */
case class D(id: String)

class DocPathTest extends FunSuite {

  test("level 1") {
    val m = Map("foo" -> 1)


    println(Reflector.toMap(D("foo")))

    val path = DocPath(m, List("foo"))
    assert(path.as[Int] == Some(1))


  }
}
