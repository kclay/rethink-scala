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
  implicit val intToNumeric = new ToAst[Int] {
    type TypeMember = Numeric

  }
  implicit val floatToNumeric = new ToAst[Float] {
    type TypeMember = Numeric

  }
 /* implicit val anyToTyped = new ToAst[Any] {
    type TypeMember = Var


  }*/
  implicit val docToTyped = new ToAst[Document] {
    type TypeMember = Strings

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


  object CanMap{
    def apply[From,To<:Typed,Out] = new CanMap[From,To,Out]
  }
  class CanMap[-From, -To<:Typed, Out]


  //implicit val canMapAny = new CanMap[Any, Strings, String]

  // implicit val canMapAnyInt = new CanMap[Any, Numeric, Double]
  //implicit val canMapAnyDocument = new CanMap[Any, MapTyped, Document]

  implicit val mapStringToStrings = CanMap[String, Strings, String]
  implicit val mapStringToNumeric= CanMap[String, Numeric, Int]
  implicit def mapStringToArray[T]=CanMap[String,ArrayTyped[T],T]


  implicit val mapIntToNumeric= CanMap[Int, Numeric, Int]
  implicit val mapDoubleToNumeric= CanMap[Double, Numeric, Double]
  implicit val mapFloatToNumeric= CanMap[Float, Numeric, Float]
  implicit val mapLongToNumeric= CanMap[Long, Numeric, Long]



  implicit def mapDocumentToDouble[T<:Document] = CanMap[T, Numeric, Double]
  implicit def mapDocumentToString[T<:Document] = CanMap[T,Strings,String]
  implicit def mapDocumentToAny[T<:Document] = CanMap[T,Ref,Any]


  trait FromAst[T>:Var]{
    type Raw
  }
  implicit val numericToInt = new FromAst[Numeric] {
    type Raw = Int
  }








  /*implicit val canMapAnyArray = new CanMap[Any, ArrayTyped[Any]] {
    type
  } */


  class ToFunctional[T, A >: Var](seq: Sequence[T]) {


    def map[B<:Typed, Inner](f: A => B)(implicit cm: CanMap[T, B, Inner]) = RMap[Inner](seq.underlying, FuncWrap(f))

    def reduce(base: T, f: (A, A) => Typed) = Reduce[T](seq.underlying, f, Some(base))

    def reduce(f: (A, A) => Typed) = Reduce[T](seq.underlying, f, None)
  }



    implicit def docToFunctional[T<:Document](seq: Sequence[T])=  new ToFunctional[T,Var](seq)

  //implicit def toFunctional[T,A <:Var](seq: Sequence[T])(implicit cf:CanFunctional[T,A]) = new ToFunctional[T, A, Typed](seq)
  // implicit def doubleToFunctional[T <: Double](seq: Sequence[]) = new ToFunctional[T, Numeric, Numeric](seq)

  // implicit def stringToFunctional(seq: Sequence[String]) = new ToFunctional[String, Strings, Strings](seq)

  // implicit def anyToFunctional(seq: Sequence[Any]) = new ToFunctional[Any, Var, Typed](seq)


    //implicit def toFunctional[T](seq: Sequence[T])(implicit ast: ToAst[T]) = new ToFunctional[T, ast.TypeMember, Typed](seq)

   implicit def toFunctional[T](seq: Sequence[T])(implicit ast: ToAst[T]):ToFunctional[T,ast.TypeMember] =new ToFunctional[T,ast.TypeMember](seq)



 // implicit def tableToFunctional[T <: Document](seq: Table[T]) = new ToFunctional[T, Var, Typed](seq)

  // implicit def docToFunctional[T <: Sequence[U], U, R >: Var](seq: Sequence[U])(implicit cf: CanFunctional[U, R]) = new ToFunctional[U, R, Typed](seq)


  //implicit def docToFunctional[T <: Document](seq: Sequence[T]) = new ToFunctional[T, Var, Typed](seq)


}
