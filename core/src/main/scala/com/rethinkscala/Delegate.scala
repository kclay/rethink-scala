package com.rethinkscala

import com.rethinkscala.ast.Produce
import com.rethinkscala.net._
import scala.concurrent.{Future, ExecutionContext}
import com.rethinkscala.net.AsyncResultQuery
import com.rethinkscala.net.BlockingResultQuery
import scala.Some

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/2/14
 * Time: 2:02 PM 
 */

object Delegate {
  def apply[T](produce:Produce[T],connection:Connection):Delegate[T]= connection match{
    case c:BlockingConnection=>apply(produce,c)
    case c:AsyncConnection=>apply(produce,c)
  }
  def apply[T](producer: Produce[T], connection: BlockingConnection) = new BlockingDelegate(producer, connection)

   def apply[T](producer: Produce[T], connection: AsyncConnection) = new AsyncDelegate(producer, connection)
}


trait Delegate[T]{

  type Result[O]
  type Query[Q]<:ResultResolver[Result[Q]]
  type Opt[R]

  def toQuery[R](tt: Manifest[R]):Query[R]



  def run(implicit mf: Manifest[T]):Result[T] = toQuery(mf).toResult

  def toDoc=toQuery[Document](Manifest.classType(classOf[Document])).toResult



  def as[R <: T](implicit mf: Manifest[R]) = toQuery(mf).toResult


  def toOpt(implicit tt: Manifest[T]): Opt[T]

  def asOpt[R <: T](implicit tt: Manifest[R]): Opt[R]
}

class BlockingDelegate[T](producer: Produce[T], connection: BlockingConnection) extends Delegate[T]{


  type Result[O] = ResultResolver.Blocking[O]
  type Query[R] = BlockingResultQuery[R]

  type Opt[T] = Option[T]

   def toQuery[R](tt: Manifest[R]) = BlockingResultQuery[R](producer.underlyingTerm, connection, tt, producer.underlyingOptions)



  def toOpt(implicit tt: Manifest[T]) = as[T].fold(x=>None,x=>Option(x))

  def asOpt[R <: T](implicit tt: Manifest[R]) = as[R].fold(x => None, x => Option(x))

    def async: AsyncDelegate[T] = Delegate(producer, AsyncConnection(connection))

}


class AsyncDelegate[T](producer: Produce[T], connection: AsyncConnection) extends Delegate[T]{

  implicit val exc: ExecutionContext = connection.version.executionContext
  type Result[O] = ResultResolver.Async[O]
  type Query[R] = AsyncResultQuery[R]

  type Opt[T] = Future[Option[T]]

  def toQuery[R](tt: Manifest[R]) = AsyncResultQuery[R](producer.underlyingTerm, connection, tt, producer.underlyingOptions)



  def toOpt(implicit mf: Manifest[T]) = as[T] transform(x => Option(x), t => t)

  def asOpt[R <: T](implicit tt: Manifest[R]) = as[R] transform(x => Option(x), t => t)


  def block: BlockingDelegate[T] = Delegate(producer, BlockingConnection(connection))
}

