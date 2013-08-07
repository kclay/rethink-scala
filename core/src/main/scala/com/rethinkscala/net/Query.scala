package com.rethinkscala.net

import scala.concurrent._
import scala.concurrent.duration.Duration
import com.rethinkscala.Term
import scala.util.Success
import scala.util.Failure
import scala.Some

abstract class Query[R] {

  val connection: Connection

  def toResult[R]: Either[RethinkError, R]

}

case class BlockingQuery[R](term: Term, connection: Connection, mf: Manifest[R]) extends Query[R] {
  def iterator: Iterator[R] = ???

  lazy val ast: ql2.Term = term.ast

  def toResult[R] = toResult(connection.timeoutDuration)

  def toResult[R](atMost: Duration): Either[RethinkError, R] = {

    val p = connection.write(term)(mf)


    try {
      Await.ready(p.future, atMost)
    } catch {
      case e: Exception => p.failure(e)

    }


    val v = p.future.value


    val r = v match {
      case Some(Failure(e: RethinkError)) => Left(e)
      case Some(Success(r)) => r match {
        case x: Option[Nothing] => Left(RethinkNoResultsError("No results found for " + mf.runtimeClass.getSimpleName, term))
        case _ => Right(r.asInstanceOf[R])


      }

      case Some(Failure(e: Exception)) => Left(RethinkRuntimeError(e.getMessage, term))
      case _ => Left(RethinkRuntimeError("Opps", term))
    }
    r

  }

}






