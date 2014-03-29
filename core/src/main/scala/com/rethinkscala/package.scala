package com

import com.rethinkscala.ast._
import com.rethinkscala.net._
import scala.concurrent.Future
import com.rethinkscala.magnets.ToNameReceptaclePimps


/** Created by IntelliJ IDEA.
  * User: Keyston
  * Date: 3/19/13
  * Time: 7:32 PM
  */
package object rethinkscala extends ImplicitConversions with ToNameReceptaclePimps {


  private[rethinkscala] trait FilterTyped


  implicit val stringToStrings = new ToAst[String] {
    type TypeMember = Strings


  }
  implicit val doubleToNumeric = new ToAst[Double] {
    type TypeMember = Numeric


  }
  implicit val intToNumeric = new ToAst[Int] {
    type TypeMember = Numeric

  }
  implicit val floatToNumeric = new ToAst[Float] {
    type TypeMember = Numeric

  }

  implicit def arrayMapToTyped[T] = new ToAst[Map[String, T]] {
    type TypeMember = Var
  }

  implicit def docToTyped[T <: Document] = new ToAst[T] {
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


  object CanMap {
    def apply[From, To <: Typed, Out] = new CanMap[From, To, Out]
  }

  class CanMap[-From, -To <: Typed, Out]


  //implicit val canMapAny = new CanMap[Any, Strings, String]

  // implicit val canMapAnyInt = new CanMap[Any, Numeric, Double]
  //implicit val canMapAnyDocument = new CanMap[Any, MapTyped, Document]

  implicit val mapStringToStrings = CanMap[String, Strings, String]
  implicit val mapStringToNumeric = CanMap[String, Numeric, Int]

  implicit def mapStringToArray[T] = CanMap[String, ArrayTyped[T], T]

  implicit def mapMapToArray[T] = CanMap[Map[String, _], ArrayTyped[T], T]


  implicit val mapIntToNumeric = CanMap[Int, Numeric, Int]
  implicit val mapDoubleToNumeric = CanMap[Double, Numeric, Double]
  implicit val mapFloatToNumeric = CanMap[Float, Numeric, Float]
  implicit val mapLongToNumeric = CanMap[Long, Numeric, Long]


  implicit def mapDocumentToDouble[T <: Document] = CanMap[T, Numeric, Double]

  implicit def mapDocumentToString[T <: Document] = CanMap[T, Strings, String]

  implicit def mapDocumentToAny[T <: Document] = CanMap[T, Ref, Any]


  trait FromAst[T >: Var] {
    type Raw
  }

  implicit val numericToDouble = new FromAst[Numeric] {
    type Raw = Double
  }
  implicit val stringsToString = new FromAst[Strings] {
    type Raw = String
  }

  implicit def arrayToSeq[T] = new FromAst[ArrayTyped[_]] {
    type Raw = Seq[T]
  }

  implicit def binaryToBoolean = new FromAst[Binary] {
    type Raw = Boolean
  }


  class ToFunctional[T, A >: Var](seq: Sequence[T]) {


    def concatMap[B <: Typed, Inner](f: A => B)(implicit cm: CanMap[T, B, Inner]) = ConcatMap[Inner](seq.underlying, FuncWrap(f))

    def map[B <: Typed, Inner](f: A => B)(implicit cm: CanMap[T, B, Inner]) = RMap[Inner](seq.underlying, FuncWrap(f))


    def reduce(f: (A, A) => Typed) = Reduce[T](seq.underlying, f)

  }


  implicit def docToFunctional[T <: Document](seq: Sequence[T]) = new ToFunctional[T, Var](seq)


  implicit def toFunctional[T](seq: Sequence[T])(implicit ast: ToAst[T]): ToFunctional[T, ast.TypeMember] = new ToFunctional[T, ast.TypeMember](seq)


  type Var = com.rethinkscala.ast.Var

  object Expr extends com.rethinkscala.ast.Expr

}
