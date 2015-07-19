package com.rethinkscala.backend

import com.rethinkscala.backend.netty.async.AsyncDelegate
import com.rethinkscala.backend.netty.blocking.BlockingDelegate
import com.rethinkscala.backend.netty.ConnectionPool

import scala.concurrent.Promise
import scala.language.{implicitConversions, higherKinds}

import com.rethinkscala._
import com.rethinkscala.Implicits.Common
import com.rethinkscala.ast.Produce
import com.rethinkscala.net._

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/15/2015
 * Time: 4:09 PM 
 */
trait RethinkBackend {

  type Result[T]
  type ConnectionDef <: Connection
  type Creator <: ConnectionCreator {type ConnectionType = ConnectionDef}

  type ProfileDef <: Common with QueryMode[ConnectionDef]
  val profile: ProfileDef


  val Connection: Creator
}

trait QueryMode[C <: Connection] {

  type DelegateDef[T] <: Delegate[T]

  def apply(version: Version): C

  type Producer[T] <: Produce[_]

  def apply[T](produce: Producer[T])(implicit connection: C): DelegateDef[T]


}


object Delegate {
  def apply[T](produce: Produce[T], connection: Connection): Delegate[T] = connection match {
    case c: BlockingConnection => apply(produce, c)
    case c: AsyncConnection => apply(produce, c)
  }

  def apply[T](producer: Produce[T], connection: BlockingConnection): BlockingDelegate[T] = new BlockingDelegate(producer, connection)

  def apply[T](producer: Produce[T], connection: AsyncConnection): AsyncDelegate[T] = new AsyncDelegate(producer, connection)
}

trait Delegate[T] {
  self =>

  type Extractor[R]
  type Result[O]
  type ResolverResult[R]
  type Query[Q] <: ResultResolver[ResolverResult[Q]]

  val connectionId: Option[Long]

  def withConnection(id: Long): Delegate[T]

  def toQuery[R](extractor: Extractor[R]): Query[R]

  def run(implicit extractor: Extractor[T]): Result[T] = toQuery(extractor)
    .toResult.asInstanceOf[Result[T]]


}

trait CastAbleDelegate[T] {
  self: Delegate[T] =>
  type Opt[R]

  def as[R <: T](implicit extractor: Extractor[R]): Result[R] = toQuery(extractor).toResult.asInstanceOf[Result[R]]

  def toOpt(implicit extractor: Extractor[T]): Opt[T]

  def asOpt[R <: T](implicit extractor: Extractor[R]): Opt[R]
}


trait RethinkConnection {
  self: Connection =>

  protected[rethinkscala] val pool: ConnectionPool
  val version: Version

  def toAst(term: Term): CompiledAst = version.toAst(term)

  def write[T](term: Term, opts: Map[String, Any], connectionId: Option[Long] = None)(implicit extractor: ResultExtractor[T]): Promise[T]

  val resultExtractorFactory: ResultExtractorFactory = new ResultExtractorFactory
}

trait ConnectionOps[C <: Connection, D <: QueryMode[C]] {
  self: C =>


  val delegate: D


  def newQuery[R](term: Term, extractor: ResultExtractor[R], opts: Map[String, Any]): ResultQuery[R]

}


trait ConnectionCreator {
  type ConnectionType <: Connection

  def apply(connection: Connection): ConnectionType
}

trait Connection extends RethinkConnection