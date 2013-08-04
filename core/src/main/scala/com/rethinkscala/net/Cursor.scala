package com.rethinkscala.net

import com.rethinkscala.ast.Sequence

import scala.collection.generic.SeqForwarder


//http://stackoverflow.com/questions/14299454/create-a-custom-scala-collection-where-map-defaults-to-returning-the-custom-coll
/*object Cursor extends TraversableFactory[Cursor] {
  def newBuilder[A] = new CustomCollectionBuilder[A]
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Cursor[A]] =
    new CanBuildFrom[Coll, A, Cursor[A]] {
      def apply(): Builder[A, Cursor[A]] = new CustomCollectionBuilder()
      def apply(from: Coll): Builder[A, Cursor[A]] = apply()
    }
}


class CustomCollectionBuilder[A] extends Builder[A, Cursor[A]] {
  private val list = new ListBuffer[A]()
  def += (elem: A): this.type = {
    list += elem
    this
  }
  def clear() {list.clear()}
  def result(): Cursor[A] = Cursor(list.result())
}              */
class Cursor[A](connectionId: Int, token: Token, chunk: Seq[A], completed: Boolean) extends SeqForwarder[A] {


  val _underlying = collection.mutable.Buffer.empty[A] ++= chunk


  // override def companion: GenericCompanion[Cursor] = Cursor
  // def foreach[U](f: A => U) { underlying foreach f }

  lazy val qt: QueryToken[A] = token.asInstanceOf[QueryToken[A]]
  // TODO need to assocate collection with the connectionid it came from
  implicit lazy val connection = qt.connection
  lazy val _size = {
    if (completed) underlying.size
    else {
      val seq = qt.term.asInstanceOf[Sequence]

      seq.count.run match {
        case Left(e: RethinkError) => underlying.size
        case Right(b: Double) => b.toInt
      }
    }


  }

  override def size = _size

  protected override def underlying = _underlying
}
