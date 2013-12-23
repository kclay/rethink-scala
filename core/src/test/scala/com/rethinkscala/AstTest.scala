package com.rethinkscala

import org.scalatest.FunSuite
import com.rethinkscala.WithBase
import com.rethinkscala.ChangeResult

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 8/13/13
 * Time: 8:55 AM 
 */
class AstTest extends FunSuite with WithBase {

  import com.rethinkscala.r

  import com.rethinkscala.Implicits._

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
