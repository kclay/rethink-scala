package com.rethinkscala.net

import ql2.{Ql2 => ql2}
import scala.concurrent._
import scala.concurrent.duration.Duration
import com.rethinkscala.{ResultExtractor, Term}
import scala.util.Success
import scala.util.Failure
import scala.Some
import scala.concurrent.Future


object ResultResolver {
  type Async[T] = Future[T]
  type Blocking[T] = Either[RethinkError, T]
}

trait ResultResolver[Result] {

  def toResult[R]: Result
}

trait ResultQuery[T] {

  type ResolveType = Either[RethinkError, T]

  val connection: Connection

  val extractor: ResultExtractor[T]
  val term: Term


  protected def resolve(t: Any): ResolveType = {
    t match {

      case Some(Failure(e: Exception)) => resolve(e)

      case Failure(e: RethinkError) => resolve(e)
      case Failure(e: Exception) => Left(RethinkRuntimeError(e.getMessage, term))
      case e: RethinkError => Left(e)
      case Some(Success(res)) => res match {
        case x: None.type => Left(RethinkNoResultsError("No results found for " + extractor.manifest.runtimeClass.getSimpleName, term))
        case _ => Right(res.asInstanceOf[T])


      }
      case value => Right(value.asInstanceOf[T])
    }
  }
}


case class AsyncResultQuery[R](term: Term, connection: AsyncConnection, extractor: ResultExtractor[R],
                               opts: Map[String, Any],connectionId:Option[Long]=None)
  extends ResultQuery[R] with ResultResolver[Future[R]] {


  implicit val exc: ExecutionContext = connection.version.executionContext

  def toResult[T] = run[T]

  protected def run[T] = {


    val p = connection.underlying.write(term, opts,connectionId)(extractor)
    // FIXME : Write this better
    p.future.transform(t => resolve(t).right.get, e => resolve(e).left.get)


  }
}


case class BlockingResultQuery[R](term: Term, connection: BlockingConnection, extractor: ResultExtractor[R],
                                  opts: Map[String, Any],connectionId:Option[Long]=None)
  extends ResultQuery[R] with ResultResolver[Either[RethinkError, R]] {


  def toResult[T] = toResult(connection.timeoutDuration)

  def toResult[T](atMost: Duration): Either[RethinkError, R] = {

    val p = connection.underlying.write(term, opts,connectionId)(extractor)


    try {
      Await.ready(p.future, atMost)
    } catch {
      case e: TimeoutException => p failure RethinkTimeoutError(e.getMessage, term)
      case e: InterruptedException => p failure RethinkClientError(e.getMessage, term)


    }

    resolve(p.future.value)


  }

}






