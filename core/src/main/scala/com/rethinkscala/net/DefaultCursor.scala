package com.rethinkscala.net


import com.rethinkscala.ast.Sequence

import scala.collection.generic.SeqForwarder


trait AbstractCursor{
  type ChunkType
}
trait RethinkCursor[T] extends AbstractCursor{
  val connectionId: Int
  type ChunkType = T

  var _chunks = collection.mutable.Seq.empty[ChunkType]
  val token: Token[_]
  // TODO need to assocate collection with the connectionid it came from
  implicit lazy val connection = BlockingConnection(token.connection)

  def completed: Boolean

  private[rethinkscala] def <<(chunks: Seq[ChunkType]) = {
    _chunks ++= chunks
    this
  }

  private[rethinkscala] def <(chunk: ChunkType) = {
    _chunks :+ chunk
    this
  }
}

//http://stackoverflow.com/questions/14299454/create-a-custom-scala-collection-where-map-defaults-to-returning-the-custom-coll

case class DefaultCursor[T](connectionId: Int, token: Token[_], completed: Boolean) extends SeqForwarder[T] with RethinkCursor[T] {


  //implicit lazy val connection = BlockingConnection(token.connection)
  lazy val _size = {
    if (completed) underlying.size
    else {
      val seq = token.term.asInstanceOf[Sequence[_]]

      import com.rethinkscala.net.Blocking._


      seq.count.run(token.extractor.to[Double]) match {
        case Left(e: RethinkError) => underlying.size
        case Right(b: Double) => b.toInt
      }
    }


  }

  override def size = _size

  protected override def underlying = _chunks
}
