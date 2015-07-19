package com.rethinkscala.backend.netty.blocking

import com.rethinkscala.ast.Produce
import com.rethinkscala.backend.netty.async.{AsyncBackend, AsyncConnection}
import com.rethinkscala.backend.netty.{AbstractConnection, ForwardingConnection}
import com.rethinkscala.backend.{Connection, ConnectionCreator, ConnectionOps}
import com.rethinkscala.net.Version
import com.rethinkscala.{ResultExtractor, Term}

import scala.concurrent.Future
import scala.concurrent.duration.Duration

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/15/2015
 * Time: 4:24 PM 
 */

trait BlockingConnectionCreator extends ConnectionCreator {


  override type ConnectionType = BlockingConnection
  val defaultTimeoutDuration = Duration(30, "seconds")

  def apply(connection: Connection): BlockingConnection = connection match {
    case c: BlockingConnection => c
    case c: AsyncConnection => build(connection.version, defaultTimeoutDuration, Some(connection))
  }

  def apply(version: Version, timeoutDuration: Duration = defaultTimeoutDuration):BlockingConnection = build(version, timeoutDuration, None)

  private def build(v: Version, t: Duration, under: Option[Connection]): BlockingConnection = under match {
    case Some(c) => new ForwardingConnection(c) with BlockingConnection {
      val timeoutDuration = t
    }
    case _ => new AbstractConnection(v) with BlockingConnection {
      val timeoutDuration = t
    }
  }
}

object BlockingConnection extends BlockingConnectionCreator


trait BlockingConnection extends Connection with ConnectionOps[BlockingConnection, BlockingProfile] {


  val delegate = BlockingProfile
  type AsyncResult[T] = AsyncBackend.Result[T]

  // FIXME : Need to place here to help out Intellijd
  def apply[T](produce: Produce[T])(implicit extractor: ResultExtractor[T]) = delegate(produce)(this).run


  def toOpt[T](produce: Produce[T])(implicit extractor: ResultExtractor[T]) = delegate(produce)(this).toOpt

  val timeoutDuration: Duration

  def toAsync: AsyncConnection = AsyncConnection(this)

  def async[T](p: Produce[T])(implicit extractor: ResultExtractor[T]): AsyncResult[T] = async(_.apply(p))

  def async[T](f: AsyncConnection => Future[T]): AsyncResult[T] = f(toAsync)

  def newQuery[R](term: Term, extractor: ResultExtractor[R], opts: Map[String, Any]) = BlockingQuery[R](term, this, extractor, opts)
}