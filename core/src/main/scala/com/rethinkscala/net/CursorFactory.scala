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

  def apply[T](token:Token[_],connectionId:Long,completed:Boolean):RethinkCursor[T]
}

object DefaultCursorFactory extends CursorFactory{

  private val cache = CacheBuilder.newBuilder()
    .concurrencyLevel(4)

    .expireAfterAccess(10, TimeUnit.MINUTES)
    .build().asInstanceOf[Cache[Long,DefaultCursor[_]]]
  override def apply[T](token: Token[_],connectionId: Long,completed:Boolean) = {

    val cursor = cache.get(token.id,new Callable[DefaultCursor[T]] {
      override def call() = DefaultCursor[T](connectionId,token)
    })

    cursor._completed = completed

    cursor.asInstanceOf[DefaultCursor[T]]
  }
}
