package com.rethinkscala.net


import com.rethinkscala.ResultExtractor
import com.rethinkscala.ast.{internal, Aggregation}
import com.rethinkscala.backend.netty.blocking.BlockingConnection
import com.typesafe.scalalogging.slf4j.LazyLogging


case class RethinkIterator[T](cursor: RethinkCursor[T]) extends Iterator[T] with LazyLogging {

  var index = 0

  override def hasNext = index < cursor.length


  override def next() = {
    val value = if (index < cursor.chunks.length) cursor.chunks(index)
    else {
      import com.rethinkscala.Blocking._
      import cursor.connection

      val more = internal.Continue[T](cursor.token.id)
      // FIXME : Currently this only works for non change feed sequences
      val extractor = cursor.token.extractor
        .asInstanceOf[ResultExtractor[RethinkCursor[T]]]
      more.withConnection(cursor.connectionId).run(extractor)
      cursor.chunks(index)
    }
    index += 1
    value
  }


}

trait RethinkCursor[T] extends Seq[T] {
  type ChunkType = T

  val isChangeFeed = false
  private[rethinkscala] val token: Token[_]

  // TODO need to assocate collection with the connectionid it came from
  implicit lazy val connection = BlockingConnection(token.connection)

  override def length = _length

  var _completed: Boolean = false

  def completed: Boolean = _completed

  lazy val _length = {

    if (completed) chunks.size
    else {
      val seq = token.term.asInstanceOf[Aggregation[_]]

      import com.rethinkscala.Blocking._

      seq.count().run(token.extractor.to[Double]) match {
        case Left(e) => chunks.size
        case Right(b) => b.toInt
      }
    }


  }

  override def apply(idx: Int) = chunks.apply(idx)


  override def iterator: Iterator[T] = new RethinkIterator[T](this)

  val connectionId: Long


  private[rethinkscala] var chunks = collection.mutable.Seq.empty[ChunkType]


  private[rethinkscala] def <<(more: Seq[ChunkType]) = {
    chunks ++= more
    this
  }

  private[rethinkscala] def <(chunk: ChunkType) = {
    chunks :+ chunk
    this
  }
}


//http://stackoverflow.com/questions/14299454/create-a-custom-scala-collection-where-map-defaults-to-returning-the-custom-coll

case class DefaultCursor[T](connectionId: Long, token: Token[_]) extends RethinkCursor[T]

