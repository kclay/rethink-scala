package com.rethinkscala

import com.rethinkscala._
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

    type Producer[T] <: Produce[_]

    def apply[T](produce: Producer[T])(implicit connection: C): D[T]


  }

  abstract class Async extends Mode[AsyncConnection] {

    def apply(version: Version) = AsyncConnection(version)
  }

  trait AsyncImplicits extends Async with AsyncCommonImplicits {
    implicit def toDelegate[T](produce: Produce[T])(implicit connection: AsyncConnection): AsyncDelegate[T] = Delegate(produce, connection)

    type D[T] = AsyncDelegate[T]

    type Producer[T] = Produce[T]
    override def apply[T](produce: Produce[T])(implicit connection: AsyncConnection): AsyncDelegate[T] = toDelegate(produce)
  }

  trait AsyncCommonImplicits {
    implicit def toAsyncResultExtractor[T: Manifest](implicit connection: AsyncConnection): ResultExtractor[T] = connection
      .resultExtractorFactory
      .create[T]
  }

  object Async extends AsyncImplicits


  abstract class Blocking extends Mode[BlockingConnection] {

    def apply(version: Version) = BlockingConnection(version)
  }

  trait BlockingImplicits extends Blocking with BlockingCommonImplicits {
    implicit def toDelegate[T](produce: Produce[T])(implicit connection: BlockingConnection): BlockingDelegate[T] = Delegate(produce, connection)

    type D[T] = BlockingDelegate[T]
    type Producer[T] = Produce[T]

    override def apply[T](produce: Produce[T])(implicit connection: BlockingConnection): BlockingDelegate[T] = toDelegate(produce)
  }

  trait BlockingCommonImplicits {


    implicit def toBlockingResultExtractor[T: Manifest](implicit connection: BlockingConnection): ResultExtractor[T] = connection
      .resultExtractorFactory
      .create[T]

  }

  trait BlockingFunctionalImplicits extends Blocking with BlockingCommonImplicits {
    implicit def toDelegate[T](produce: Produce[T])(implicit connection: BlockingConnection): BlockingTryDelegate[T] = BlockingTryDelegate(Delegate(produce, connection))

    type D[T] = BlockingTryDelegate[T]
    type Producer[T] = Produce[T]

    override def apply[T](produce: Produce[T])(implicit connection: BlockingConnection) = toDelegate(produce)


  }

  object Blocking extends BlockingImplicits {

    object experimental extends BlockingFunctionalImplicits

  }

}
