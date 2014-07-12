package com

import com.rethinkscala.ast._
import com.rethinkscala.net._

import scala.concurrent.Future


/** Created by IntelliJ IDEA.
  * User: Keyston
  * Date: 3/19/13
  * Time: 7:32 PM
  */
package object rethinkscala extends ImplicitConversions {


  private[rethinkscala] trait FilterTyped



  type BlockResult[T] = ResultResolver.Blocking[T]
  type AsyncResult[T] = ResultResolver.Async[T]

  def async[T](p:Produce[T])(implicit c:Connection,mf:Manifest[T]):AsyncResult[T]=async(_.apply(p))
  def async[T](f: AsyncConnection => Future[T])(implicit c: Connection):AsyncResult[T] = f(AsyncConnection(c))

  def block[T](f: BlockingConnection => Unit)(implicit c: Connection):Unit = f(BlockingConnection(c))
  def block[T](f: BlockingConnection => BlockResult[T])(implicit c: Connection):BlockResult[T] = f(BlockingConnection(c))
  def block[T](p:Produce[T])(implicit c:Connection,mf:Manifest[T]):BlockResult[T] = {
    def inner(c:BlockingConnection):BlockResult[T] = c.apply(p)
    block(inner _)
  }


  trait FromAst[T] {
    type Raw
  }

  implicit val numericToDouble = new FromAst[Numeric] {
    type Raw = Double
  }
  implicit val stringsToString = new FromAst[Strings] {
    type Raw = String
  }

  implicit def arrayToSeq[T](seq: ProduceSequence[T]) = new FromAst[ProduceSequence[T]] {
    type Raw = Seq[T]
  }

  implicit def binaryToBoolean = new FromAst[Binary] {
    type Raw = Boolean
  }


  type Var = com.rethinkscala.ast.Var
  val Expr = com.rethinkscala.ast.Expr
  val Blocking = com.rethinkscala.Implicits.Blocking
  val Async = com.rethinkscala.Implicits.Async
  type BlockingConnection = com.rethinkscala.net.BlockingConnection
  type AsyncConnection = com.rethinkscala.net.AsyncConnection


}
