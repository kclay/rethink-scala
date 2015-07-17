package com.rethinkscala.changefeeds.net

import com.rethinkscala.{ResultExtractor, CursorChange}
import com.rethinkscala.ast.internal
import com.rethinkscala.net.{RethinkIterator, RethinkCursor, Token}
import com.typesafe.scalalogging.slf4j.LazyLogging

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 8:45 PM
 *
 */


case class RethinkChangesIterator[T](cursor: ChangeCursor[T]) extends Iterator[CursorChange[T]] {


  override def hasNext = true


  def fetch(): Unit = {
    import com.rethinkscala.Blocking._
    import cursor.connection

    val more = internal.Continue[CursorChange[T]](cursor.token.id)

    /* val changeExtractor = cursor.token.extractor.to[ChangeCursor[CursorChange[T]]]
       .asInstanceOf[ResultExtractor[RethinkCursor[CursorChange[T]]]]*/
    // FIXME : Currently this only works for non change feed sequences
    val mf = cursor.token.extractor.manifest
    val extractor = cursor.token.extractor
      .asInstanceOf[ResultExtractor[RethinkCursor[CursorChange[T]]]]

    val result = more.withConnection(cursor.connectionId).toOpt(extractor)
    result.foreach {
      case res: ChangeCursor[_] => println("Queue Size = " + res.queue.size)
    }

  }


  override def next() = {
    try {
      cursor.queue.dequeue()
    } catch {
      case t: NoSuchElementException => fetch()
        cursor.queue.dequeue()
      case t: Throwable => throw t

    }
  }


}

case class ChangeCursor[T](connectionId: Long, token: Token[_]) extends RethinkCursor[CursorChange[T]] {


  override val isChangeFeed: Boolean = true

  val queue = new scala.collection.mutable.Queue[CursorChange[T]]


  override def iterator: Iterator[CursorChange[T]] = new RethinkChangesIterator[T](this)

  def stop = synchronized {
    _completed = true
  }

  override private[rethinkscala] def <<(more: Seq[ChunkType]) = {
    queue ++= more
    this
  }

  override private[rethinkscala] def <(chunk: ChunkType) = {
    queue += chunk
    this
  }

  override def completed = synchronized(_completed)

  override def toString() = s"ChangeCursor($connectionId,$token)"
}