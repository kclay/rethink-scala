package com.rethinkscala

import org.scalatest.FunSuite
import Blocking._

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 11/3/2014
 * Time: 10:46 AM 
 */
class UUIDTest extends FunSuite with WithBase {

  test("uuid") {

    assert(r.uuid.run, {
      u: Any => u.isInstanceOf[java.util.UUID]
    })
    assert(r.uuid.string.run, {
      u: Any => u.isInstanceOf[String]
    })
  }
}
