package com.rethinkdb

/** Created by IntelliJ IDEA.
 *  User: Keyston
 *  Date: 3/26/13
 *  Time: 8:21 PM
 */

sealed trait FrameType

case object PositionFrame extends FrameType

case object OptionalFrame extends FrameType

case class Frame(frameType: Option[FrameType], pos: Option[Long], opt: Option[String]) {

}

abstract class RethinkError(message: String) extends Exception(message) {

  val term: Term
  val frames: Iterable[Frame]
}

//abstract class RethinkError(message:String,term:Term,frames:Iterable[Frame]) extends Exception(message)
case class RethinkRuntimeError(message: String, term: Term, frames: Iterable[Frame] = Iterable.empty[Frame]) extends RethinkError(message)

case class RethinkCompileError(message: String, term: Term, frames: Iterable[Frame]) extends RethinkError(message)

case class RethinkClientError(message: String, term: Term, frames: Iterable[Frame]) extends RethinkError(message)

