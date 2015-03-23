package com.rethinkscala

import org.scalatest.FunSuite
import Blocking._

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 3/22/15
 * Time: 5:43 PM
 *
 */

case class ChangeFeedEntity(id:Option[String],value:String)
class ChangeFeedTest  extends FunSuite with WithBase{


  test("change feeds"){

    val feed = table.to[ChangeFeedEntity]

    //feed.changes.ru
  }
}
