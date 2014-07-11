package com.rethinkscala.net


import com.rethinkscala.ast.Sequence

import scala.collection.generic.SeqForwarder


//http://stackoverflow.com/questions/14299454/create-a-custom-scala-collection-where-map-defaults-to-returning-the-custom-coll

class Cursor[A](connectionId: Int, token: Token[_], chunk: Seq[A], completed: Boolean) extends SeqForwarder[A] {


  val _underlying = chunk

  // TODO need to assocate collection with the connectionid it came from
  implicit lazy val connection = BlockingConnection(token.connection)
  lazy val _size = {
    if (completed) underlying.size
    else {
      val seq = token.term.asInstanceOf[Sequence[_]]

      import com.rethinkscala.net.Blocking._



      seq.count.run match {
        case Left(e: RethinkError) => underlying.size
        case Right(b: Double) => b.toInt
      }
    }


  }

  override def size = _size

  protected override def underlying = chunk
}
