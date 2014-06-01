package com.rethinkscala

import com.rethinkscala.net.{BlockingConnection, Version2}
import com.rethinkscala._
import com.rethinkscala.reflect.Reflector

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 3/29/14
 * Time: 12:30 PM
 *
 */
object GroupDebug {

  /*
  val host = (Option(scala.util.Properties.envOrElse("TRAVIS", "empty")) map {
    case "empty" => "172.16.2.45"
    case _ => "127.0.0.1"
  }).get
  val port = 28015
  val authKey = "foobar"

  val version2 = new Version2(host, port, authKey = authKey)

  implicit val connection: BlockingConnection = BlockingConnection(version2)


  def main(args: Array[String]): Unit = {

    import connection.delegate._

    val json = """[
                 |    {"id": 2, "player": "Bob", "points": 15, "type": "ranked"},
                 |    {"id": 5, "player": "Alice", "points": 7, "type": "free"},
                 |    {"id": 11, "player": "Bob", "points": 10, "type": "free"},
                 |    {"id": 12, "player": "Alice", "points": 2, "type": "free"}
                 |]""".stripMargin

    val seq = Expr(Reflector.fromJson[Seq[Map[String, Any]]](json))
    val results = seq.group("points".as[Int]).run











    System.out.println(results.fold(e=>e.getCause,x=>x))
    System.exit(0)

  }
  */
}
