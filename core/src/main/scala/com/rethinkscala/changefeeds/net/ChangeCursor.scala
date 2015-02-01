package com.rethinkscala.changefeeds.net

import com.rethinkscala.CursorChange
import com.rethinkscala.net.{RethinkCursor, Token}

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 8:45 PM
 *
 */


class ChangeWatcher[T](f: CursorChange[T] => Unit, cursor: ChangeCursor[T]) {
  def cancel: Unit = cursor.watchers - this

  def apply() = cancel

  private[rethinkscala] def apply(change: CursorChange[T]) = f(change)
}

case class ChangeCursor[T](connectionId: Int, token: Token[_]) extends RethinkCursor[CursorChange[T]] {


  private[rethinkscala] var watchers = collection.mutable.ArrayBuffer.empty[ChangeWatcher[T]]


  override private[rethinkscala] def <<(chunks: Seq[ChunkType]) = {
    chunks.foreach(chunk => watchers.foreach(_.apply(chunk)))
    this
  }

  override private[rethinkscala] def <(chunk: ChunkType) = {
    watchers.foreach(_.apply(chunk))
    this
  }

  var _completed = false

  def stop = synchronized {
    _completed = true
  }

  def completed = synchronized(_completed)

  def foreach(f: ChunkType => Unit) = {
    val watcher = new ChangeWatcher(f, this)
    watchers :+ watcher
    watcher
  }

  def apply(f: ChunkType => Unit) = foreach(f)
}