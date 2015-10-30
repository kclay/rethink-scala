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

case object UnknownFrame extends FrameType

case object PositionFrame extends FrameType

abstract class RethinkError(message: String) extends Exception(message)

case class RethinkDriverError(message: String) extends RethinkError(message)

@deprecated("Use ReqlClientError", "0.4.8")
case class RethinkClientError(message: String, term: Term, frames: Iterable[Frame] = Iterable.empty[Frame]) extends ReqlError(message, term, frames)

case class ReqlClientError(message: String, term: Term, frames: Iterable[Frame] = Iterable.empty[Frame],underlying: Option[Throwable] = None) extends ReqlError(message, term, frames)

case class Frame(frameType: Option[FrameType], pos: Option[Long], opt: Option[String])

@deprecated("Use ReqlCompileError", "0.4.8")
case class RethinkCompileError(message: String, term: Term, frames: Iterable[Frame]) extends ReqlError(message, term, frames)


@deprecated("Use ReqlNoResultsError", "0.4.8")
case class RethinkNoResultsError(message: String, term: Term, frames: Iterable[Frame] = Iterable.empty[Frame]) extends ReqlError(message, term, frames)

case class ReqlNoResultsError(message: String, term: Term, frames: Iterable[Frame] = Iterable.empty[Frame]) extends ReqlError(message, term, frames)

@deprecated("Use ReqlRuntimeError", "0.4.8")
case class RethinkRuntimeError(message: String, term: Term, frames: Iterable[Frame] = Iterable.empty[Frame], underlying: Option[Throwable] = None)
  extends ReqlRuntimeError(message, term, frames, underlying)

@deprecated("Use ReqlTimeoutError", "0.4.8")
case class RethinkTimeoutError(message: String, term: Term, frames: Iterable[Frame] = Iterable.empty) extends RethinkError(message)

case class ReqlTimeoutError(message: String, term: Term, frames: Iterable[Frame] = Iterable.empty[Frame]) extends ReqlError(message, term, frames)



abstract class ReqlError(message: String, term: Term, frames: Iterable[Frame]) extends Exception(message)

object ReqlError {

  //private [this] val errorTypeToClass
  //def byId(id:Int)=
}

case class ReqlCompileError(message: String, term: Term, frames: Iterable[Frame]) extends ReqlError(message, term, frames)

abstract class ReqlRuntimeError(message: String, term: Term, frames: Iterable[Frame], underlying: Option[Throwable] = None) extends ReqlError(message, term, frames)


object ReqlRuntimeError {

  def unapply(e: ReqlError):Option[(String, Term, Iterable[Frame])] = e match {
    case ReqlQueryLogicError(m, t, f) => Some((m, t, f))
    case ReqlNonExistenceError(m, t, f) => Some((m, t, f))
    case ReqlResourceLimitError(m, t, f) => Some((m, t, f))
    case ReqlUserError(m, t, f) => Some((m, t, f))
    case ReqlInternalError(m, t, f) => Some((m, t, f))
    case ReqlAvailabilityError(m, t, f) => Some((m, t, f))
    case _ => None
  }
}



case class ReqlQueryLogicError(message: String, term: Term, frames: Iterable[Frame]) extends ReqlRuntimeError(message, term, frames)

case class ReqlNonExistenceError(message: String, term: Term, frames: Iterable[Frame]) extends ReqlRuntimeError(message, term, frames)

case class ReqlResourceLimitError(message: String, term: Term, frames: Iterable[Frame]) extends ReqlRuntimeError(message, term, frames)

case class ReqlUserError(message: String, term: Term, frames: Iterable[Frame]) extends ReqlRuntimeError(message, term, frames)

case class ReqlInternalError(message: String, term: Term, frames: Iterable[Frame]) extends ReqlRuntimeError(message, term, frames)

abstract class ReqlAvailabilityError(message: String, term: Term, frames: Iterable[Frame]) extends ReqlRuntimeError(message, term, frames)

object ReqlAvailabilityError {
  def unapply(e: ReqlError): Option[(String, Term, Iterable[Frame])] = e match {
    case ReqlOpFailedError(m, t, f) => Some((m, t, f))
    case ReqlOpIndeterminateError(m, t, f) => Some((m, t, f))
    case _ => None
  }
}

case class ReqlOpFailedError(message: String, term: Term, frames: Iterable[Frame]) extends ReqlRuntimeError(message, term, frames)

case class ReqlOpIndeterminateError(message: String, term: Term, frames: Iterable[Frame]) extends ReqlRuntimeError(message, term, frames)


abstract class ReqlDriverError(message: String, term: Term, frames: Iterable[Frame]) extends ReqlRuntimeError(message, term, frames)

object ReqlDriverError {
  def unapply(e: ReqlError) = e match {
    case ReqlAuthError(m, t, f) => Some((m, t, f))
    case _ =>
  }
}

case class ReqlAuthError(message: String, term: Term, frames: Iterable[Frame]) extends ReqlDriverError(message, term, frames)


