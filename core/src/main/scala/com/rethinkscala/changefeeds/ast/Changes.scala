package com.rethinkscala.changefeeds.ast

import java.util.concurrent.atomic.AtomicInteger

import com.rethinkscala._
import com.rethinkscala.ast.{Produce, Typed, ProduceChangeStream}
import com.rethinkscala.changefeeds.net.ChangeCursor
import com.rethinkscala.net.BlockingConnection
import ql2.Ql2.Term.TermType
import com.rethinkscala.net._

import scalaz.concurrent.Task
import scalaz._
import scalaz.\/._
import scalaz.stream.Process

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 2/1/15
 * Time: 11:15 AM
 *
 */


case class Changes[T](target: Typed) extends ProduceChangeStream[T] {


  //override lazy val optargs: Iterable[AssocPair] = buildOptArgs(Map("include_states" -> Some(true)))

  override def termType = TermType.CHANGES
}

case class ChangeFeedDelegate[T](producer: Produce[ChangeCursor[CursorChange[T]]], connection: Connection, connectionId: Option[Long] = None)
  extends Delegate[T] {
  lazy val underlying = BlockingDelegate(producer, BlockingConnection(connection), connectionId)

  type Cursor[R] = ChangeCursor[R]
  type Extractor[R] = ResultExtractor[Cursor[R]]


  override def withConnection(id: Long) = copy(connectionId = Some(id))

  override def toQuery[R](extractor: Extractor[R]) = underlying.toQuery(extractor)

  type Result[O] = Process[Task, CursorChange[O]]
  type ResolverResult[R] = ResultResolver.Blocking[Cursor[R]]
  type Query[R] = BlockingResultQuery[Cursor[R]]


  override def run(implicit extractor: Extractor[T]) = {

    val task = ChangeFeedResult(this, extractor).task
    Process.eval(task).collect {
      case Some(c) => c
    }.repeat
  }
}


case class ChangeFeedResult[T](delegate: ChangeFeedDelegate[T],
                               extractor: ResultExtractor[ChangeCursor[T]]) {

  var iterator: Option[Iterator[CursorChange[T]]] = None

  def task: Task[Option[CursorChange[T]]] = {

    def work(callback: Throwable \/ Option[CursorChange[T]] => Unit): Unit = {
      try {

        iterator = iterator.orElse(
          delegate.toQuery(extractor)
            .toResult.fold(_ => None, Some(_)).map(_.iterator)
        )
        iterator.foreach(it => callback(right(Some(it.next()))))


      }
      catch {
        case nse: NoSuchElementException => callback(right(None))
        case t: Throwable => callback(left(t))
      }
    }
    Task.async(work)
  }


}

trait BlockingChangeFeedImplicits extends Blocking with BlockingCommonImplicits {
  type Producer[T] = Produce[ChangeCursor[CursorChange[T]]]

  implicit def toChangeFeedDelegate[T](produce: Producer[T])(implicit connection: BlockingConnection): ChangeFeedDelegate[T]
  = ChangeFeedDelegate(produce, connection)

  type D[T] = ChangeFeedDelegate[T]

  override def apply[T](produce: Producer[T])(implicit connection: BlockingConnection): ChangeFeedDelegate[T] = ChangeFeedDelegate(produce, connection)
}
