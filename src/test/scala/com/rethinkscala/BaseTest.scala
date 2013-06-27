package com.rethinkscala

import org.scalatest.FunSuite
import com.rethinkscala.ast.Produce

/** Created by IntelliJ IDEA.
 *  User: Keyston
 *  Date: 6/18/13
 *  Time: 3:45 PM
 */

import ql2._
import com.rethinkscala.Implicits.Quick._

trait BaseTest {
  self: FunSuite =>
  val host = (Option(scala.util.Properties.envOrElse("TRAVIS", "empty")) map {
    case "empty" => "172.16.2.45"
    case _       => "127.0.0.1"
  }).get
  val port = 28015
  val authKey = ""
  val version1 = new Version1(host, port)
  val version2 = new Version2(host, port, authKey = authKey)

  def useVersion = version1

  type IS = Iterable[String]
  type IA = Iterable[Any]
  implicit val connection: Connection = new Connection(useVersion)

  def assert(t: ql2.Term, tt: Term.TermType.EnumVal) {
    assert(t.`type`.get == tt)
  }

  def assert(d: Option[ql2.Datum], value: String) {
    assert(d.get.str == value)
  }

  def assert(d: ql2.Datum, value: String) {
    assert(d.str == value)
  }

  def assert[Result](query: Produce[Result], testValue: Result)(implicit mf: Manifest[Result]) {
    assert(query.run.fold(x => false, y => y == testValue))
  }

  def assert[Result](query: Produce[Result], check: Result => Boolean)(implicit mf: Manifest[Result]) {

    assert(query.run.fold(x => false, y => check(y)))
  }

  def assertAs[Result <: Document](query: Produce[Document], check: Result => Boolean)(implicit mf: Manifest[Result]) {

    assert(query.as[Result].fold(x => false, y => check(y)))
  }


}
