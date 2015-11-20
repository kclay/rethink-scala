package com.rethinkscala.backend.netty.blocking


import com.rethinkscala._
import com.rethinkscala.ast.Produce
import com.rethinkscala.backend.Delegate
import com.rethinkscala.changefeeds.net.ChangeCursor

import com.rethinkscala.backend.{Connection => BackendConnection}

import scalaz.\/._
import scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/16/2015
 * Time: 9:09 AM 
 */
case class ChangeFeedDelegate[T](producer: Produce[ChangeCursor[CursorChange[T]]],
                                 connection: BackendConnection, connectionId: Option[Long] = None)
  extends Delegate[T] {
  lazy val underlying = BlockingDelegate(producer, BlockingConnection(connection), connectionId)

  type Cursor[R] = ChangeCursor[R]
  type Extractor[R] = ResultExtractor[Cursor[R]]


  override def withConnection(id: Long) = copy(connectionId = Some(id))

  override def toQuery[R](extractor: Extractor[R]) = underlying.toQuery(extractor)

  type Result[O] = Process[Task, CursorChange[O]]
  type ResolverResult[R] = BlockingBackend.Result[Cursor[R]]
  type Query[R] = BlockingQuery[Cursor[R]]


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
