package com.rethinkscala

import com.rethinkscala.ast.Produce
import com.rethinkscala.changefeeds.net.ChangeCursor
import com.rethinkscala.net.JsonCursorResponse
import com.rethinkscala.reflect.Reflector
import org.scalatest.FunSuite
import Blocking._
import scalaz.stream.io

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 3/22/15
 * Time: 5:43 PM
 *
 */

case class ChangeFeedEntity(id: Option[String], value: String)

class ChangeFeedTest extends FunSuite with WithBase {


  test("change feeds") {

    val feed = table.to[ChangeFeedEntity]


    val changes: Produce[ChangeCursor[CursorChange[ChangeFeedEntity]]] = feed.changes
    val process = changes.run.map(_.toString).run.run

    Thread.sleep(2 * 1000)

    println(feed.insert(ChangeFeedEntity(None, "foo")).toOpt)

    Thread.sleep(2 * 1000)
    import scalaz.concurrent.Task
    import scalaz._
    import scalaz.\/._
    import scalaz.stream.Process
    // Process.range(1,10,2).toSource.runLog.run

  }

  override lazy val dbName: String = "test"
  override lazy val tableName: String = "changefeeds"
}

