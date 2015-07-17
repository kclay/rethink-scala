package com.rethinkscala.backend.netty.async

import com.rethinkscala._
import com.rethinkscala.backend.{Delegate, CastAbleDelegate}
import com.rethinkscala.ast.Produce
import com.rethinkscala.backend.netty.blocking.{BlockingConnection, BlockingDelegate}

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/15/2015
 * Time: 5:16 PM 
 */
case class AsyncDelegate[T](producer: Produce[T], connection: AsyncConnection, connectionId: Option[Long] = None)
  extends Delegate[T] with CastAbleDelegate[T] {

  implicit val exc: ExecutionContext = connection.version.executionContext
  type Extractor[R] = ResultExtractor[R]
  type Result[O] = AsyncBackend.Result[O]
  type ResolverResult[R] = Result[R]
  type Query[R] = AsyncQuery[R]
  type Opt[T] = Future[Option[T]]

  def withConnection(id: Long) = copy(connectionId = Some(id))

  def toQuery[R](extractor: Extractor[R]): Query[R] = AsyncQuery[R](producer.underlyingTerm, connection, extractor,
    producer.underlyingOptions, connectionId)

  def toOpt(implicit extractor: Extractor[T]): Future[Option[T]] = as[T] transform(x => Option(x), t => t)

  def asOpt[R <: T](implicit extractor: Extractor[R]): Future[Option[R]] = as[R] transform(x => Option(x), t => t)


  def block: BlockingDelegate[T] = new BlockingDelegate[T](producer, BlockingConnection(connection))
}