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

  protected val cache = CacheBuilder.newBuilder()
    .concurrencyLevel(4)
    .expireAfterAccess(10, TimeUnit.MINUTES)
    .build().asInstanceOf[Cache[Long, RethinkCursor[_]]]

  type CursorType[T] <: RethinkCursor[_]

  def newCallable[T](connectionId: Long, token: Token[_]): Callable[CursorType[T]]

  def apply[T](token: Token[_], connectionId: Long, completed: Boolean): CursorType[T] = {

    val cursor = cache.get(token.id, newCallable[T](connectionId, token))

    cursor._completed = completed

    cursor.asInstanceOf[CursorType[T]]

  }
}

object DefaultCursorFactory extends CursorFactory {

  type CursorType[T] = RethinkCursor[T]

  def newCallable[T](connectionId: Long, token: Token[_]) = new Callable[RethinkCursor[T]] {
    override def call() = DefaultCursor[T](connectionId, token)
  }

}
