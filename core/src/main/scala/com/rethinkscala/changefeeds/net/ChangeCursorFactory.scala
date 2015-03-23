package com.rethinkscala.changefeeds.net

import java.util.concurrent.Callable

import com.rethinkscala.net.{CursorFactory, Token}

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 8:59 PM
 *
 */
object ChangeCursorFactory extends CursorFactory {
  type CursorType[T] = ChangeCursor[T]

  def newCallable[T](connectionId: Long, token: Token[_]) = new Callable[ChangeCursor[T]] {
    override def call() = ChangeCursor[T](connectionId, token)
  }
}
