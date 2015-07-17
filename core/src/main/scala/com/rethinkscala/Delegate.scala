package com.rethinkscala

import com.rethinkscala.ast.Produce
import com.rethinkscala.backend.netty.async.AsyncDelegate
import com.rethinkscala.backend.netty.blocking.BlockingDelegate
import com.rethinkscala.net._

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/2/14
 * Time: 2:02 PM 
 */


case class ResultExtractor[T](cursorFactory: CursorFactory, manifest: Manifest[T]) {

  def to[R: Manifest] = ResultExtractor[R](cursorFactory, implicitly[Manifest[R]])

}

object Delegate {
  def apply[T](produce: Produce[T], connection: Connection): backend.Delegate[T] = connection match {
    case c: BlockingConnection => apply(produce, c)
    case c: AsyncConnection => apply(produce, c)
  }

  def apply[T](producer: Produce[T], connection: BlockingConnection): BlockingDelegate[T] = new BlockingDelegate(producer, connection)

  def apply[T](producer: Produce[T], connection: AsyncConnection): AsyncDelegate[T] = new AsyncDelegate(producer, connection)
}



