package com.rethinkscala

import com.rethinkscala.ast.Produce
import com.rethinkscala.Delegate


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 6/1/14
 * Time: 11:53 AM
 *
 */
package object net {


  trait Mode[C<:Connection]{


    type D[T] <:Delegate[T]
    def apply(version: Version):C

    def apply[T](produce: Produce[T])(implicit connection:C):D[T]


  }
  abstract class Async extends Mode[AsyncConnection]{

    def apply(version: Version) = AsyncConnection(version)
  }
  object Async extends Async{
    implicit def toDelegate[T](produce: Produce[T])(implicit connection: AsyncConnection) = Delegate(produce,connection)

    type D[T] = AsyncDelegate[T]

    override def apply[T](produce: Produce[T])(implicit connection: AsyncConnection) = toDelegate(produce)
  }


  abstract class Blocking extends Mode[BlockingConnection]{

    def apply(version: Version) = BlockingConnection(version)
  }
  object Blocking extends Blocking {
    implicit def toDelegate[T](produce: Produce[T])(implicit connection: BlockingConnection) = Delegate(produce,connection)

    type D[T] = BlockingDelegate[T]

    override def apply[T](produce: Produce[T])(implicit connection: BlockingConnection) = toDelegate(produce)
  }
}
