package com.rethinkscala.net

import java.util.concurrent.{Callable, TimeUnit}

import com.google.common.cache.{Cache, CacheBuilder}

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 8:46 PM
 *
 */
trait CursorFactory {


  val cache: Cache[Long, RethinkCursor[_]]

  type CursorType[T] <: RethinkCursor[_]

  def newCallable[T](connectionId: Long, token: Token[_]): Callable[CursorType[T]]

  def apply[T](token: Token[_], connectionId: Long, completed: Boolean): CursorType[T] = {

    val cursor = cache.get(token.id, newCallable[T](connectionId, token))

    cursor._completed = completed

    cursor.asInstanceOf[CursorType[T]]

  }
}

case class DefaultCursorFactory(cache: Cache[Long, RethinkCursor[_]]) extends CursorFactory {

  type CursorType[T] = RethinkCursor[T]

  def newCallable[T](connectionId: Long, token: Token[_]) = new Callable[RethinkCursor[T]] {
    override def call() = DefaultCursor[T](connectionId, token)
  }

}
