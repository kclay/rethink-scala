package com.rethinkscala

import com.rethinkscala.ast.Produce


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 6/1/14
 * Time: 11:53 AM
 *
 */
package object net {


  trait Mode[C <: Connection] {

    type D[T] <: Delegate[T]

    def apply(version: Version): C

    def apply[T](produce: Produce[T])(implicit connection: C): D[T]


  }

  abstract class Async extends Mode[AsyncConnection] {

    def apply(version: Version) = AsyncConnection(version)
  }

  trait AsyncImplicits extends Async {
    implicit def toDelegate[T](produce: Produce[T])(implicit connection: AsyncConnection): AsyncDelegate[T] = Delegate(produce, connection)

    type D[T] = AsyncDelegate[T]

    override def apply[T](produce: Produce[T])(implicit connection: AsyncConnection): AsyncDelegate[T] = toDelegate(produce)
  }

  object Async extends AsyncImplicits


  abstract class Blocking extends Mode[BlockingConnection] {

    def apply(version: Version) = BlockingConnection(version)
  }

  trait BlockingImplicits extends Blocking {
    implicit def toDelegate[T](produce: Produce[T])(implicit connection: BlockingConnection): BlockingDelegate[T] = Delegate(produce, connection)

    type D[T] = BlockingDelegate[T]

    override def apply[T](produce: Produce[T])(implicit connection: BlockingConnection): BlockingDelegate[T] = toDelegate(produce)
  }

  trait BlockingFunctionalImplicits extends Blocking {
    implicit def toDelegate[T](produce: Produce[T])(implicit connection: BlockingConnection): BlockingTryDelegate[T] = BlockingTryDelegate(Delegate(produce, connection))

    type D[T] = BlockingTryDelegate[T]

    override def apply[T](produce: Produce[T])(implicit connection: BlockingConnection) = toDelegate(produce)


  }

  object Blocking extends BlockingImplicits {

    object experimental extends BlockingFunctionalImplicits

  }

}
