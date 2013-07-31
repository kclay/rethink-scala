package com.rethinkscala.net

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Success, Failure}
import com.rethinkscala.Term

abstract class Query[R] {

  val connection: Connection

  def toResult[R]: Either[RethinkError, R]

}

case class BlockingQuery[R](term: Term, connection: Connection, mf: Manifest[R]) extends Query[R] {
  def iterator: Iterator[R] = ???

  lazy val ast: ql2.Term = term.ast

  def toResult[R] = toResult(Duration(20, "seconds"))

  def toResult[R](atMost: Duration): Either[RethinkError, R] = {

    val f = connection.write(term)(mf)

    Await.ready(f, atMost)

    val v = f.value
    val itClass = classOf[Iterable[R]]


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






