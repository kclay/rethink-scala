package com.rethinkscala.net

import ql2.{Ql2 => ql2}
import scala.concurrent._
import scala.concurrent.duration.Duration
import com.rethinkscala.Term
import scala.util.Success
import scala.util.Failure
import scala.Some
import scala.concurrent.Future

trait ResultResolver[Result] {

  def toResult[R]: Result
}

trait ResultQuery[T] {


 // lazy val ast: ql2.Term = term.ast

  type ResolveType = Either[RethinkError, T]

  val connection: Connection

  val mf: Manifest[T]
  val term: Term


  protected def resolve(t: Any): ResolveType = {
    t match {
      case Some(Failure(e: RethinkError)) => Left(e)
      case Some(Success(res)) => res match {
        case x: None.type => Left(RethinkNoResultsError("No results found for " + mf.runtimeClass.getSimpleName, term))
        case _ => Right(res.asInstanceOf[T])


      }

      case Some(Failure(e: Exception)) => Left(RethinkRuntimeError(e.getMessage, term))
      case _ => Left(RethinkRuntimeError("Opps", term))
    }
  }
}


trait QueryMode

trait Blocking extends QueryMode


case class AsyncResultQuery[R](term: Term, connection: AsyncConnection, mf: Manifest[R], opts: Map[String, Any])
  extends ResultQuery[R] with ResultResolver[Future[R]] {


  implicit val exc: ExecutionContext = connection.version.executionContext

  def toResult[T] = run[T]

  protected def run[T] = {


    val p = connection.underlying.write(term, opts)(mf)
    p.future.transform[R](t => resolve(t).right.get, identity)


  }
}


case class BlockingResultQuery[R](term: Term, connection: BlockingConnection, mf: Manifest[R], opts: Map[String, Any])
  extends ResultQuery[R] with ResultResolver[Either[RethinkError, R]] {


  def toResult[T] = toResult(connection.timeoutDuration)

  def toResult[T](atMost: Duration): Either[RethinkError, R] = {

    val p = connection.underlying.write(term, opts)(mf)


    try {
      Await.ready(p.future, atMost)
    } catch {
      case e: TimeoutException => p failure RethinkTimeoutError(e.getMessage, term)
      case e: InterruptedException => p failure RethinkClientError(e.getMessage, term)


    }

    resolve(p.future.value)


  }

}






