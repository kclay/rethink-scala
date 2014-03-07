package com.rethinkscala

import com.rethinkscala.ast.Produce
import com.rethinkscala.net.{AsyncResultQuery, BlockingResultQuery, BlockingConnection, AsyncConnection}
import scala.concurrent.ExecutionContext

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/2/14
 * Time: 2:02 PM 
 */

object Delegate {
  def apply[T](producer: Produce[T], connection: BlockingConnection) = new BlockingDelegate(producer, connection)

  def apply[T](producer: Produce[T], connection: AsyncConnection) = new AsyncDelegate(producer, connection)
}


class BlockingDelegate[T](producer: Produce[T], connection: BlockingConnection) {


  def toQuery[R <: T](tt: Manifest[R]) = BlockingResultQuery[R](producer.underlyingTerm, connection, tt, producer.underlyingOptions)

  def run(implicit mf: Manifest[T]) = toQuery(mf).toResult


  def as[R <: T](implicit mf: Manifest[R]) = toQuery(mf).toResult


  def toOpt(implicit tt: Manifest[T]): Option[T] = as[T].fold(x => None, x => Some(x))

  def asOpt[R <: T](implicit tt: Manifest[R]): Option[R] = as[R].fold(x => None, x => Option(x))

  def async: AsyncDelegate[T] = Delegate(producer, AsyncConnection(connection))

}

class AsyncDelegate[T](producer: Produce[T], connection: AsyncConnection) {

  implicit val exc: ExecutionContext = connection.version.executionContext


  def toQuery[R <: T](tt: Manifest[R]) = AsyncResultQuery[R](producer.underlyingTerm, connection, tt, producer.underlyingOptions)

  def run(implicit mf: Manifest[T]) = toQuery(mf).toResult

  def as[R <: T](implicit mf: Manifest[R]) = toQuery(mf).toResult

  def toOpt(implicit mf: Manifest[T]) = as[T] transform(x => Option(x), t => t)

  def asOpt[R <: T](implicit tt: Manifest[R]) = as[R] transform(x => Option(x), t => t)

  def block: BlockingDelegate[T] = Delegate(producer, BlockingConnection(connection))
}

/*
object Blocking {

 object Connection {
   def apply(version: Version): BlockingConnection = BlockingConnection(version)

 }

 implicit class BlockingDelegate[T](producer: Produce[T])(implicit connection: BlockingConnection) extends QueryDelegate[
   T, Either[RethinkError, T], BlockingResultQuery[_], BlockingConnection](producer, connection) {


   def toQuery[R <: T](tt: Manifest[R]) = BlockingResultQuery[R](producer.underlyingTerm, connection, tt, producer.underlyingOptions)

   def run(implicit mf: Manifest[T]) = toQuery(mf).toResult


   def as[R <: T](implicit mf: Manifest[R]) = toQuery(mf).toResult


   def toOpt(implicit tt: Manifest[T]): Option[T] = as[T] fold(x => None, x => Some(x))

   def asOpt[R <: T](implicit tt: Manifest[R]): Option[R] = as[R].fold(x => None, x => Option(x))


 }

}

object Async {

 object Connection {
   def apply(version: Version) = AsyncConnection(version)

 }

 implicit class AsyncDelegate[T](producer: Produce[T])(implicit connection: AsyncConnection) extends QueryDelegate[
   T,
   Future[T],
   AsyncResultQuery[_],
   AsyncConnection](producer, connection) {

   implicit val exc: ExecutionContext = connection.version.executionContext


   def toQuery[R <: T](tt: Manifest[R]) = AsyncResultQuery[R](producer.underlyingTerm, connection, tt, producer.underlyingOptions)

   def run(implicit mf: Manifest[T]) = toQuery(mf).toResult

   def as[R <: T](implicit mf: Manifest[R]) = toQuery(mf).toResult

   def toOpt(implicit mf: Manifest[T]) = as[T] transform(x => Option(x), t => t)

   def asOpt[R <: T](implicit tt: Manifest[R]) = as[R] transform(x => Option(x), t => t)
 }

}     */
