package com.rethinkscala.backend.netty.async

import com.rethinkscala.ast.Produce
import com.rethinkscala.backend.netty.{ForwardingConnection, AbstractConnection}
import com.rethinkscala.backend.netty.blocking.{BlockingBackend, BlockingConnection}
import com.rethinkscala.backend.{Connection, ConnectionCreator, ConnectionOps}
import com.rethinkscala.net.Version
import com.rethinkscala.{ResultExtractor, Term}

import scala.concurrent.ExecutionContext

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/15/2015
 * Time: 5:15 PM 
 */


trait AsyncConnectionCreator extends ConnectionCreator {

  override type ConnectionType = AsyncConnection

  def apply(version: Version) = build(version, None)


  def apply(connection: Connection): AsyncConnection = connection match {
    case c: AsyncConnection => c
    case c: BlockingConnection => build(connection.version, Some(connection))
  }


  private def build(v: Version, under: Option[Connection]): AsyncConnection = under match {
    case Some(c) => new ForwardingConnection(c) with AsyncConnection

    case _ => new AbstractConnection(v) with AsyncConnection
  }
}


object AsyncConnection extends AsyncConnectionCreator

trait AsyncConnection extends Connection with ConnectionOps[AsyncConnection, AsyncProfile] {

  val delegate = AsyncProfile
  type BlockingResult[T] = BlockingBackend.Result[T]

  implicit val executionContext: ExecutionContext = version.executionContext

  // FIXME : Need to place here to help out Intellij with async(_.apply(res))
  def apply[T](produce: Produce[T])(implicit extractor: ResultExtractor[T]) = delegate(produce)(this).run

  def toOpt[T](produce: Produce[T])(implicit extractor: ResultExtractor[T]) = delegate(produce)(this).toOpt

  def newQuery[R](term: Term, extractor: ResultExtractor[R], opts: Map[String, Any]) = AsyncQuery[R](term, this, extractor, opts)

  def toBlocking: BlockingConnection = BlockingConnection(this)

  def block[T](f: BlockingConnection => Unit): Unit = f(toBlocking)

  def block[T](f: BlockingConnection => BlockingResult[T]): BlockingResult[T] = f(toBlocking)

  def block[T](p: Produce[T])(implicit extractor: ResultExtractor[T]): BlockingResult[T] = {

    block { c: BlockingConnection => c.apply(p)(extractor) }
  }

}