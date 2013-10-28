package com.rethinkscala.ast

import org.scalatest.FunSuite
import com.rethinkscala._

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 10/28/13
 * Time: 10:19 AM 
 */
class SequenceTest extends FunSuite with WithBase {

  test("contains") {


    table.contains("2", {
      v: Var => 1
    })
  }
}
