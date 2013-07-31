package com.rethinkscala.net

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Try, Success, Failure}
import com.rethinkscala.Term
import scala.concurrent.{Future, promise}

abstract class Query[R] {

  type T = Either[RethinkError, R]
  type Result
  val connection: Connection
  val mf: Manifest[R]
  val term: Term


  def toResult[R]: Result

  protected def resolve(t: Any): T = {

    def extract(v: Any) = v match {
      case x: Option[Nothing] => Left(RethinkNoResultsError("No results found for " + mf.runtimeClass.getSimpleName, term))
      case _ => Right(v.asInstanceOf[R])


    }


    t match {
      case Failure(e: RethinkError) => Left(e)
      case Some(Failure(e: RethinkError)) => Left(e)
      case Success(r) => extract(r)
      case Some(Success(r)) => extract(r)
      case Some(Failure(e: Exception)) => Left(RethinkRuntimeError(e.getMessage, term))
      case _ => Left(RethinkRuntimeError("Opps", term))
    }
  }

}

trait QueryMode {

  def apply[R](term: Term, connection: Connection, mf: Manifest[R]): Query[R]
}

object Blocking extends Blocking

class Blocking extends QueryMode {
  override def apply[R](term: Term, connection: Connection, mf: Manifest[R]) = {
    BlockingQuery[R](term, connection, mf)
  }
}

object Async extends Async

class Async extends QueryMode {

  override def apply[R](term: Term, connection: Connection, mf: Manifest[R]) = {
    AsyncQuery[R](term, connection, mf)
  }
}


case class AsyncQuery[R](term: Term, connection: Connection, mf: Manifest[R]) extends Query[R] {

  type Result = Future[T]

  import scala.concurrent.ExecutionContext.Implicits._

  def toResult[R] = run[R]

  protected def run[R] = {
    val p = promise[T]
    val f = connection.write(term)(mf)

    f onComplete resolve
    p.future


  }
}

case class BlockingQuery[R](term: Term, connection: Connection, mf: Manifest[R]) extends Query[R] {

  type Result = T
  lazy val ast: ql2.Term = term.ast

  def toResult[R] = toResult(Duration(20, "seconds"))


  def toResult[R](atMost: Duration): Result = {

    val f = connection.write(term)(mf)

    Await.ready(f, atMost)


    resolve(f.value)


  }

}






