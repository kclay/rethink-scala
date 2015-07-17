package com.rethinkscala.backend.netty.blocking

import com.rethinkscala.Implicits.Common
import com.rethinkscala.ast.Produce
import com.rethinkscala.backend.QueryMode
import com.rethinkscala.backend.netty.blocking
import com.rethinkscala.changefeeds.net.ChangeCursor
import com.rethinkscala.net.Version
import com.rethinkscala._

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/15/2015
 * Time: 4:30 PM 
 */


trait LowPriorityCommon {


  implicit def toBlockingResultExtractor[T: Manifest](implicit connection: BlockingConnection): ResultExtractor[T] = connection
    .resultExtractorFactory
    .create[T]

}

trait BlockingQueryMode extends QueryMode[BlockingConnection] {
  def apply(version: Version) = BlockingConnection(version)

  implicit def toDelegate[T](produce: Produce[T])(implicit connection: blocking.BlockingConnection): BlockingDelegate[T] = new BlockingDelegate(produce, connection)

  type DelegateDef[T] = BlockingDelegate[T]
  type Producer[T] = Produce[T]


  override def apply[T](produce: Produce[T])(implicit connection: blocking.BlockingConnection): BlockingDelegate[T] = toDelegate(produce)

}

trait BlockingProfile extends LowPriorityCommon with BlockingQueryMode with Common {

   type ChangeProducer[T] = Produce[ChangeCursor[CursorChange[T]]]

   implicit def toChangeFeedDelegate[T](produce: ChangeProducer[T])(implicit connection: BlockingConnection): ChangeFeedDelegate[T]
   = ChangeFeedDelegate(produce, connection)

  /*trait BlockingChangeFeedImplicits extends Blocking with BlockingCommonImplicits {
    type Producer[T] = Produce[ChangeCursor[CursorChange[T]]]

    implicit def toChangeFeedDelegate[T](produce: Producer[T])(implicit connection: BlockingConnection): ChangeFeedDelegate[T]
    = ChangeFeedDelegate(produce, connection)

    type D[T] = ChangeFeedDelegate[T]

    override def apply[T](produce: Producer[T])(implicit connection: BlockingConnection): ChangeFeedDelegate[T] = ChangeFeedDelegate(produce, connection)
  }  */

}


trait BlockingFunctionalQueryMode extends QueryMode[BlockingConnection] {
  def apply(version: Version) = BlockingConnection(version)

  implicit def toDelegate[T](produce: Produce[T])(implicit connection: BlockingConnection): BlockingTryDelegate[T] = BlockingTryDelegate(Delegate(produce, connection))

  type DelegateDef[T] = BlockingTryDelegate[T]
  type Producer[T] = Produce[T]

  override def apply[T](produce: Produce[T])(implicit connection: BlockingConnection) = toDelegate(produce)
}

trait BlockingFunctionalImplicits extends LowPriorityCommon with BlockingFunctionalQueryMode with Common


object BlockingProfile extends BlockingProfile {

  object experimental extends BlockingFunctionalImplicits

  object functional extends BlockingFunctionalImplicits

}

