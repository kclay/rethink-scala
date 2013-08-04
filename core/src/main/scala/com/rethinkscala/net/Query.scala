package com.rethinkscala.net

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Try, Success, Failure}
import com.rethinkscala.Term
import scala.concurrent.{Future, promise}

abstract class Query[R] {


  type Result
  val connection: Connection
  val mf: Manifest[R]
  val term: Term


  def toResult: Result


}


case class AsyncQuery[R](term: Term, connection: Connection, mf: Manifest[R]) extends Query[R] {

  type Result = Future[R]

  import scala.concurrent.ExecutionContext.Implicits._

  def toResult = connection.write(term)(mf) transform(s => s,
    f => f match {
      case e: RethinkError => e
      case e: Exception => RethinkRuntimeError(e.getMessage, term)
    })


}

case class BlockingQuery[R](term: Term, connection: Connection, mf: Manifest[R]) extends Query[R] {

  type Result = Either[Exception, R]
  lazy val ast: ql2.Term = term.ast

  def toResult = toResult(Duration(20, "seconds"))


  def toResult(atMost: Duration): Result = {

    val f = connection.write(term)(mf)

    Await.ready(f, atMost)


    f.value match {

      case Some(Failure(e: RethinkError)) => Left(e)

      case Some(Success(r)) => Right(r.asInstanceOf[R])

      case Some(Failure(e: Exception)) => Left(RethinkRuntimeError(e.getMessage, term))
      case _ => Left(RethinkRuntimeError("Opps", term))
    }


  }

}






