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

  abstract class QueryDelegate[ProduceType, ResultType, QueryType <: ResultResolver[ResultType],
  ConnectionType <: QueryFactory](producer: Produce[ProduceType], connection: ConnectionType) {

    def toQuery[R <: ProduceType](tt: Manifest[R]): QueryType

    // def toQuery[R](tt: Manifest[R]): QueryType

    // def run(implicit mf: Manifest[ProduceType]): ResultType = toQuery(mf).toResult

    //  def as[R <: ProduceType](implicit mf: Manifest[R]) = toQuery(mf).toResult

    // def toQuery[R <: ProduceType](tt: Manifest[R]): QueryType[ResultType] = connection.newQuery[R](producer.underlyingTerm, tt, producer.underlyingOptions).asInstanceOf[QueryType[ResultType]]


  }

  object Blocking {

    def apply = this

    object Connection {
      def apply(version: Version): BlockingConnection = BlockingConnection(version)

    }

    implicit def toDelegate[T](produce: Produce[T])(implicit connection: BlockingConnection) = Delegate(produce, connection)


  }


  // def run(implicit c: Connection, mf: Manifest[T], d: DummyImplicit): Either[RethinkError, Seq[T]] = toQuery[T].toResult


  //def as[R <: T](implicit c: Connection, mf: Manifest[R], d: DummyImplicit): Either[RethinkError, Seq[R]] = toQuery[R].toResult

  //def toOpt(implicit c: Connection, mf: Manifest[T], d: DummyImplicit): Option[Seq[T]] = run fold(x => None, Some(_))

  def block[T](c: Connection)(f: BlockingConnection => T) = f(BlockingConnection(c))

  def async[T](c: Connection)(f: AsyncConnection => Future[T]) = f(AsyncConnection(c))


}
