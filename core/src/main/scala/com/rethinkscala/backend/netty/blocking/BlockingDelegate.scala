package com.rethinkscala.backend.netty.blocking

import com.rethinkscala._
import com.rethinkscala.ast.Produce
import com.rethinkscala.backend.netty.async.{AsyncConnection, AsyncDelegate}
import com.rethinkscala.backend.{CastAbleDelegate, Delegate}
import com.rethinkscala.net.{ReqlError, RethinkError}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/15/2015
 * Time: 4:48 PM 
 */
case class BlockingDelegate[T](producer: Produce[T], connection: BlockingConnection, connectionId: Option[Long] = None)
  extends Delegate[T] with CastAbleDelegate[T] {

  type Extractor[R] = ResultExtractor[R]
  type Result[O] = BlockingBackend.Result[O]
  type ResolverResult[R] = Result[R]
  type Query[R] = BlockingQuery[R]
  type Opt[R] = Option[R]


  def withConnection(id: Long) = copy(connectionId = Some(id))

  def toQuery[R](extractor: Extractor[R]): BlockingQuery[R] = BlockingQuery[R](producer.underlyingTerm, connection, extractor, producer.underlyingOptions, connectionId)

  def toOpt(implicit extractor: Extractor[T]): Option[T] = as[T].fold(x => None, x => Option(x))

  def asOpt[R <: T](implicit extractor: Extractor[R]): Option[R] = as[R].fold(x => None, x => Option(x))

  def async: AsyncDelegate[T] = new AsyncDelegate[T](producer, AsyncConnection(connection))

}

case class BlockingTryDelegate[T](delegate: BlockingDelegate[T], connectionId: Option[Long] = None)
  extends Delegate[T] with CastAbleDelegate[T] {

  import scala.util.{Failure, Success, Try}

  type Extractor[R] = ResultExtractor[R]
  type Result[O] = Try[T]
  type ResolverResult[R] = BlockingBackend.Result[R]
  type Query[R] = BlockingQuery[R]
  type Opt[R] = Try[R]


  def withConnection(id: Long) = copy(connectionId = Some(id), delegate = delegate.withConnection(id))

  def toQuery[R](extractor: Extractor[R]): BlockingQuery[R] = delegate.toQuery(extractor)

  override def run(implicit extractor: Extractor[T]): Try[T] = toQuery(extractor).toResult match {
    case Left(e) => Failure(e)
    case Right(o) => Success(o)
  }

  override def as[R <: T](implicit extractor: Extractor[R]): Try[R] = toQuery(extractor).toResult match {
    case Left(e) => Failure(e)
    case Right(o) => Success(o)
  }

  def toOpt(implicit extractor: Extractor[T]): Try[T] = run(extractor)

  def asOpt[R <: T](implicit extractor: Extractor[R]): Try[R] = as[R](extractor)


}

