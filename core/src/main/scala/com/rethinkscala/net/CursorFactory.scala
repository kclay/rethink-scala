package com.rethinkscala.net

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 8:46 PM
 *
 */
trait CursorFactory {

  def apply[T](connectionId:Int,token:Token[_],completed:Boolean):RethinkCursor
}

object DefaultCursorFactory extends CursorFactory{
  override def apply[T](connectionId: Int,token: Token[_],  completed: Boolean) = DefaultCursor[T](connectionId,token,completed)
}
