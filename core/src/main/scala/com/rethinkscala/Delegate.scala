package com.rethinkscala

import com.rethinkscala.ast.Produce
import com.rethinkscala.net.{AsyncResultQuery, BlockingResultQuery, _}

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/2/14
 * Time: 2:02 PM 
 */


case class ResultExtractor[T](cursorFactory: CursorFactory, manifest: Manifest[T]) {
  def to[R: Manifest] = ResultExtractor[R](cursorFactory, implicitly[Manifest[R]])
}

object Delegate {
  def apply[T](produce: Produce[T], connection: Connection): Delegate[T] = connection match {
    case c: BlockingConnection => apply(produce, c)
    case c: AsyncConnection => apply(produce, c)
  }

  def apply[T](producer: Produce[T], connection: BlockingConnection) = new BlockingDelegate(producer, connection)

  def apply[T](producer: Produce[T], connection: AsyncConnection) = new AsyncDelegate(producer, connection)
}


trait Delegate[T] {

  type Extractor[R] = ResultExtractor[R]
  type Result[O]
  type Query[Q] <: ResultResolver[Result[Q]]
  type Opt[R]

  def toQuery[R](extractor: Extractor[R]): Query[R]

  def run(implicit extractor: Extractor[T]) = toQuery(extractor).toResult

  def as[R <: T](implicit extractor: Extractor[R]) = toQuery(extractor).toResult

  def toOpt(implicit extractor: Extractor[T]): Opt[T]

  def asOpt[R <: T](implicit extractor: Extractor[R]): Opt[R]
}


class BlockingDelegate[T](producer: Produce[T], connection: BlockingConnection) extends Delegate[T] {

  type Result[O] = ResultResolver.Blocking[O]
  type Query[R] = BlockingResultQuery[R]
  type Opt[T] = Option[T]

  def toQuery[R](extractor: Extractor[R]):BlockingResultQuery[R] = BlockingResultQuery[R](producer.underlyingTerm, connection, extractor, producer.underlyingOptions)

  def toOpt(implicit extractor: Extractor[T]): Option[T] = as[T].fold(x => None, x => Option(x))

  def asOpt[R <: T](implicit extractor: Extractor[R]): Option[R] = as[R].fold(x => None, x => Option(x))

  def async: AsyncDelegate[T] = Delegate(producer, AsyncConnection(connection))

}


class AsyncDelegate[T](producer: Produce[T], connection: AsyncConnection) extends Delegate[T] {

  implicit val exc: ExecutionContext = connection.version.executionContext
  type Result[O] = ResultResolver.Async[O]
  type Query[R] = AsyncResultQuery[R]
  type Opt[T] = Future[Option[T]]

  def toQuery[R](extractor: Extractor[R]):AsyncResultQuery[R] = AsyncResultQuery[R](producer.underlyingTerm, connection, extractor, producer.underlyingOptions)

  def toOpt(implicit extractor: Extractor[T]):Future[Option[T]] = as[T] transform(x => Option(x), t => t)

  def asOpt[R <: T](implicit extractor: Extractor[R]):Future[Option[R]] = as[R] transform(x => Option(x), t => t)


  def block: BlockingDelegate[T] = Delegate(producer, BlockingConnection(connection))
}

