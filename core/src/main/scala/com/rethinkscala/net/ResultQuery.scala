package com.rethinkscala.net

import ql2.{Ql2 => ql2}
import scala.concurrent._
import scala.concurrent.duration.Duration
import com.rethinkscala.{ResultExtractor, Term}
import scala.util.Success
import scala.util.Failure
import scala.Some
import scala.concurrent.Future




trait ResultResolver[Result] {

  def toResult[R]: Result
}

trait ResultQuery[T] {

  type ResolveType = Either[RethinkError, T]

  val connection: Connection

  val extractor: ResultExtractor[T]
  val term: Term


  protected def resolve(t: Any): ResolveType =
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






