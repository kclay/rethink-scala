package com.rethinkscala.net

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/3/13
 * Time: 5:21 PM
 *
 */

import com.rethinkscala.Term

sealed trait FrameType

case object OptionalFrame extends FrameType

case object PositionFrame extends FrameType

abstract class RethinkError(message: String) extends Exception(message) {

  val term: Term
  val frames: Iterable[Frame]
}

case class RethinkClientError(message: String, term: Term, frames: Iterable[Frame]) extends RethinkError(message)

case class Frame(frameType: Option[FrameType], pos: Option[Long], opt: Option[String])

case class RethinkCompileError(message: String, term: Term, frames: Iterable[Frame]) extends RethinkError(message)

//abstract class RethinkError(message:String,term:Term,frames:Iterable[Frame]) extends Exception(message)
case class RethinkRuntimeError(message: String, term: Term, frames: Iterable[Frame] = Iterable.empty[Frame]) extends RethinkError(message)


