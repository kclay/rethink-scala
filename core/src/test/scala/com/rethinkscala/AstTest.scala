package com.rethinkscala

import org.scalatest.FunSuite

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 8/13/13
 * Time: 8:55 AM 
 */
class AstTest extends FunSuite with WithBase {


  /*
  test("branch") {


    val creatorId = "a"
    val opponentId = "b"
    val term = r.db("spec").table("stats").filter(s => s \ "id" === creatorId or s \ "id" === opponentId).update(
      r.branch(r.row("id") === creatorId,
        Map("wins" -> r.row("wins").add(1)),
        Map("loses" -> r.row("loses").add(1))
      ))




    assert(term, {
      cr: ChangeResult => cr.replaced == 2
    })
    // println(query)


  }  */

}
