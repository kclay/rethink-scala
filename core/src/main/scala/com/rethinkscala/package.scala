package com

import com.rethinkscala.ast._
import com.rethinkscala.net._
import scala.concurrent.{ExecutionContext, Future}
import com.rethinkscala.net.AsyncResultQuery
import com.rethinkscala.ast.Var
import com.rethinkscala.net.BlockingResultQuery
import scala.Some


/** Created by IntelliJ IDEA.
  * User: Keyston
  * Date: 3/19/13
  * Time: 7:32 PM
  */
package object rethinkscala extends ImplicitConversions {

  private[rethinkscala] trait DatumOrFunction


  implicit val stringToStrings = new ToAst[String] {
    type TypeMember = Strings
  }
  implicit val doubleToNumeric = new ToAst[Double] {
    type TypeMember = Numeric
  }
  implicit val anyToTyped = new ToAst[Any] {
    type TypeMember = Typed
  }
  implicit val docToTyped = new ToAst[Document] {
    type TypeMember = Var
  }

  object Async {

    object Connection {
      def apply(version: Version) = AsyncConnection(version)

    }

    implicit def toDelegate[T](produce: Produce[T])(implicit connection: AsyncConnection) = Delegate(produce, connection)


  }

  object Blocking {


    object Connection {
      def apply(version: Version): BlockingConnection = BlockingConnection(version)

    }

    implicit def toDelegate[T](produce: Produce[T])(implicit connection: BlockingConnection) = Delegate(produce, connection)


  }

  trait BlockingContext[T] extends Function[BlockingConnection, T]

  trait AsyncContext[T] extends Function[AsyncConnection, Future[T]]

  implicit def toBlockingContext[T](f: BlockingConnection => T) = new BlockingContext[T] {
    def apply(v1: BlockingConnection) = f(v1)
  }

  implicit def toAsyncContext[T](f: AsyncConnection => Future[T]) = new AsyncContext[T] {
    def apply(v1: AsyncConnection) = f(v1)
  }



  def block[T](c: Connection)(f: BlockingContext[T]) = f(BlockingConnection(c))

  def async[T](c: Connection)(f: AsyncContext[T]) = f(AsyncConnection(c))


}
