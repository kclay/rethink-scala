package com.rethinkscala

import org.scalatest.{ShouldMatchers, BeforeAndAfterAll, FunSuite}

import org.scalatest.exceptions.TestFailedException
import com.rethinkscala.ast.Produce
import ql2.{Ql2 => ql2}
import com.rethinkscala._
import com.rethinkscala.reflect.Reflector

/** Created by IntelliJ IDEA.
  * User: Keyston
  * Date: 6/18/13
  * Time: 3:45 PM
  */

import ql2._
import com.rethinkscala.Implicits.Quick._
import com.rethinkscala.net._
import com.rethinkscala.ast._


case class Player(id:Int,player:String,points:Int,`type`:String) extends Document

trait WithBase extends BeforeAndAfterAll with ShouldMatchers {
  self: FunSuite =>

  val host = (Option(scala.util.Properties.envOrElse("TRAVIS", "empty")) map {
    case "empty" => "172.16.2.45"
    case _ => "127.0.0.1"
  }).get
  val port = 28015
  val authKey = "foobar"

  val version2 = new Version2(host, port, authKey = authKey)
  val version3 = new Version3(host, port, authKey = authKey)

  lazy val testSeq={

    val json = """[
                 |    {"id": 2, "player": "Bob", "points": 15, "type": "ranked"},
                 |    {"id": 5, "player": "Alice", "points": 7, "type": "free"},
                 |    {"id": 11, "player": "Bob", "points": 10, "type": "free"},
                 |    {"id": 12, "player": "Alice", "points": 2, "type": "free"}
                 |]""".stripMargin
    val map = Reflector.fromJson[Seq[Map[String, Any]]](json)


     Expr(map)


  }

  type TableType = Document
  def useVersion:Version = version3

  implicit val connection: BlockingConnection = BlockingConnection(useVersion)

  import connection.delegate._


  def setupDB = true

  lazy val db = r.db(dbName)
  lazy val table = db.table[Document](tableName)





  override protected def beforeAll() {


    super.beforeAll()
    if (setupDB) {
      db.create.run
      table.create.run
    }

  }

  override protected def afterAll() {
    super.afterAll()
    if (setupDB) {
      db.drop.run
    }
  }

  lazy val dbName = randomAlphanumericString(5)

  lazy val tableName = randomAlphanumericString(5)

  // Random generator
  val random = new scala.util.Random

  // Generate a random string of length n from the given alphabet
  def randomString(alphabet: String)(n: Int): String =
    Stream.continually(random.nextInt(alphabet.size)).map(alphabet).take(n).mkString

  // Generate a random alphabnumeric string of length n
  def randomAlphanumericString(n: Int) =
    randomString("abcdefghijklmnopqrstuvwxyz0123456789")(n)


  type IS = Iterable[String]
  type IA = Iterable[Any]

  type Return[T] = Either[RethinkError,T]


  def assert(t: ql2.Term, tt: Term.TermType) {
    assert(t.getType == tt)
  }

  def assert(d: Option[ql2.Datum], value: String) {
    assert(d.get.str == value)
  }

  def assert(d: ql2.Datum, value: String) {
    assert(d.str == value)
  }

  private def assert_[Result](f: () => Either[RethinkError, Result], check: Result => Boolean)(implicit mf: Manifest[Result]) {
    val (condition, cause) = f() match {
      case Left(e) => (false, e.getMessage)
      case Right(r) => (check(r), s"Successful query but invalid response = $r")
    }
    if (!condition)
      throw new TestFailedException(cause, 5)
  }

  def assert[Result](query: Produce[Result], testValue: Result)(implicit mf: Manifest[Result]) {
    assert_[Result](() => query.run.asInstanceOf[Return[Result]], {
      r: Result => r == testValue
    })

  }
  def assert1[Result](query: Produce[Result], testValue: Result)(implicit mf: Manifest[Result]) {
    assert_[Result](() => query.run.asInstanceOf[Return[Result]], {
      r: Result => r == testValue
    })

  }

  def assert(query: Produce[Boolean]) {
    assert[Boolean](query, true)
  }


  def assert[Result](query: Produce[Result], check: Result => Boolean)(implicit mf: Manifest[Result]) {
    assert_[Result](() => query.run.asInstanceOf[Return[Result]], check)

  }



 def assertAs[Result <: Document](query: Produce[Document], check: Result => Boolean)(implicit mf: Manifest[Result]) {

   assert_[Result](() => query.as[Result].asInstanceOf[Return[Result]], check)

 }


  def assert[Result](result: Either[RethinkError, Result], check: Result => Boolean)(implicit mf: Manifest[Result]) {
    assert_[Result](() => result, check)
  }

  def assert0(condition: Boolean) {
    if (!condition)
      throw new TestFailedException(5)
  }

}
