package com.rethinkscala.net

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/3/13
 * Time: 5:21 PM
 *
 */
abstract class RethinkError(message: String) extends Exception(message) {

  val term: Term
  val frames: Iterable[Frame]
}
