package com.rethinkscala

import com.rethinkscala.ast.Produce
import com.rethinkscala.net.{AsyncResultQuery, BlockingResultQuery, _}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

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

  def apply[T](producer: Produce[T], connection: BlockingConnection): BlockingDelegate[T] = new BlockingDelegate(producer, connection)

  def apply[T](producer: Produce[T], connection: AsyncConnection): AsyncDelegate[T] = new AsyncDelegate(producer, connection)
}


trait Delegate[T] {
  self =>

  type Extractor[R]
  type Result[O]
  type ResolverResult[R]
  type Query[Q] <: ResultResolver[ResolverResult[Q]]

  val connectionId: Option[Long]

  def withConnection(id: Long): Delegate[T]

  def toQuery[R](extractor: Extractor[R]): Query[R]

  def run(implicit extractor: Extractor[T]): Result[T] = toQuery(extractor)
    .toResult.asInstanceOf[Result[T]]


}

trait CastAbleDelegate[T] {
  self: Delegate[T] =>
  type Opt[R]

  def as[R <: T](implicit extractor: Extractor[R]): Result[R] = toQuery(extractor).toResult.asInstanceOf[Result[R]]

  def toOpt(implicit extractor: Extractor[T]): Opt[T]

  def asOpt[R <: T](implicit extractor: Extractor[R]): Opt[R]
}


case class BlockingTryDelegate[T](delegate: BlockingDelegate[T], connectionId: Option[Long] = None)
  extends Delegate[T] with CastAbleDelegate[T] {

  import scala.util.{Try, Success, Failure}

  type Extractor[R] = ResultExtractor[R]
  type Result[O] = Try[T]
  type ResolverResult[R] = ResultResolver.Blocking[R]
  type Query[R] = BlockingResultQuery[R]
  type Opt[R] = Try[R]


  def withConnection(id: Long) = copy(connectionId = Some(id), delegate = delegate.withConnection(id))

  def toQuery[R](extractor: Extractor[R]): BlockingResultQuery[R] = delegate.toQuery(extractor)

  override def run(implicit extractor: Extractor[T]): Try[T] = toQuery(extractor).toResult match {
    case Left(e: RethinkError) => Failure(e)
    case Right(o) => Success(o)
  }

  override def as[R <: T](implicit extractor: Extractor[R]): Try[R] = toQuery(extractor).toResult match {
    case Left(e: RethinkError) => Failure(e)
    case Right(o) => Success(o)
  }

  def toOpt(implicit extractor: Extractor[T]): Try[T] = run(extractor)

  def asOpt[R <: T](implicit extractor: Extractor[R]): Try[R] = as[R](extractor)


}

case class BlockingDelegate[T](producer: Produce[T], connection: BlockingConnection, connectionId: Option[Long] = None)
  extends Delegate[T] with CastAbleDelegate[T] {

  type Extractor[R] = ResultExtractor[R]
  type Result[O] = ResultResolver.Blocking[O]
  type ResolverResult[R] = Result[R]
  type Query[R] = BlockingResultQuery[R]
  type Opt[R] = Option[R]


  def withConnection(id: Long) = copy(connectionId = Some(id))

  def toQuery[R](extractor: Extractor[R]): BlockingResultQuery[R] = BlockingResultQuery[R](producer.underlyingTerm, connection, extractor, producer.underlyingOptions, connectionId)

  def toOpt(implicit extractor: Extractor[T]): Option[T] = as[T].fold(x => None, x => Option(x))

  def asOpt[R <: T](implicit extractor: Extractor[R]): Option[R] = as[R].fold(x => None, x => Option(x))

  def async: AsyncDelegate[T] = Delegate(producer, AsyncConnection(connection))

}


case class AsyncDelegate[T](producer: Produce[T], connection: AsyncConnection, connectionId: Option[Long] = None)
  extends Delegate[T] with CastAbleDelegate[T] {

  implicit val exc: ExecutionContext = connection.version.executionContext
  type Extractor[R] = ResultExtractor[R]
  type Result[O] = ResultResolver.Async[O]
  type ResolverResult[R] = Result[R]
  type Query[R] = AsyncResultQuery[R]
  type Opt[T] = Future[Option[T]]

  def withConnection(id: Long) = copy(connectionId = Some(id))

  def toQuery[R](extractor: Extractor[R]): AsyncResultQuery[R] = AsyncResultQuery[R](producer.underlyingTerm, connection, extractor,
    producer.underlyingOptions, connectionId)

  def toOpt(implicit extractor: Extractor[T]): Future[Option[T]] = as[T] transform(x => Option(x), t => t)

  def asOpt[R <: T](implicit extractor: Extractor[R]): Future[Option[R]] = as[R] transform(x => Option(x), t => t)


  def block: BlockingDelegate[T] = Delegate(producer, BlockingConnection(connection))
}

