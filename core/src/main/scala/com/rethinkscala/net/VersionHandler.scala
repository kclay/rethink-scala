package com.rethinkscala.net

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong

import com.google.common.cache.{Cache, CacheBuilder}
import com.rethinkscala.ast.{ProduceSequence, internal}
import com.rethinkscala.{ResultExtractor, Term}
import com.typesafe.scalalogging.slf4j.LazyLogging
import ql2.Ql2.Response
import ql2.Ql2.Response.ResponseType
import ql2.Ql2.Response.ResponseType._

import scala.concurrent.Promise
import scala.util.{Failure, Success, Try}

/**
  * Created by IntelliJ IDEA.
  * User: Keyston
  * Date: 8/22/2014
  * Time: 2:22 PM
  */
trait VersionHandler[R] extends LazyLogging {
  val version: Version

  private val newTokenId: AtomicLong = new AtomicLong()

  def handle(tokenId: Long, response: R): Unit

  type TokenType <: Token[_]


  override def toString: String = {
    s"VersionHandler($version)"
  }

  def failure(e: Throwable) = {
    logger.error("UnCaught Exception token not resolved", e)
  }

  private[rethinkscala] val tokensById: Cache[Long, TokenType] = CacheBuilder.newBuilder()
    .concurrencyLevel(4)

    .expireAfterWrite(5, TimeUnit.MINUTES)
    .build().asInstanceOf[Cache[Long, TokenType]]


  def newQuery[T](conn: Connection, connectionId: Long, term: Term,
                  promise: Promise[T], db: Option[String] = None, opts: Map[String, Any] = Map())
                 (implicit extractor: ResultExtractor[T]) = {


    val tokenId = term match {
      case internal.Continue(token) => token

      case _ => newTokenId.incrementAndGet()

    }

    val query = version.toQuery[T](term, tokenId, db, opts)
    val token = query.asToken(conn, connectionId, term, promise)
    tokensById.put(tokenId, token.asInstanceOf[TokenType])
    query
  }

  def handle[T](id: Long)(f: TokenType => Unit): Unit = {

    Option(tokensById.getIfPresent(id)) match {
      case Some(token) => Try(f(token)) match {
        case Failure(e) => logger.error("Error in trying to handle token", e)

          throw ReqlClientError(e.getMessage, token.term, Iterable.empty, Some(e))//)
          // FIXME : Find a better way to invalidate the connection before resolving token
        /*  try {
            throw e
          } catch {
            case _: Throwable => //throw e
          } finally {

          }*/
        case Success(res) => logger.debug(s"Results = $res")
      }
      case _ => logger.error(s"No Token found for $id")
    }


  }
}

case class JsonVersionHandler(version: Version) extends VersionHandler[String] {

  type TokenType = JsonQueryToken[_]
  val ResponseTypeExtractor = """"t":(\d+)""".r.unanchored

  private def process(token: TokenType, json: String, responseType: Int) = responseType match {
    case RUNTIME_ERROR_VALUE | COMPILE_ERROR_VALUE | CLIENT_ERROR_VALUE => token.toError(json)
    case SUCCESS_PARTIAL_VALUE | SUCCESS_SEQUENCE_VALUE => token.toCursor(json, responseType.toInt)
    case SUCCESS_ATOM_VALUE => token.term match {
      case x: ProduceSequence[_] => token.toCursor(json, responseType.toInt, atom = true)
      case _ => token.toResult(json)
    }
    case _ => RethinkRuntimeError(s"Invalid response = $json", token.term)
  }

  override def handle(tokenId: Long, json: String): Unit = {
    handle(tokenId) {
      token => (json match {
        case ResponseTypeExtractor(responseType) => process(token, json, responseType.toInt)
        case _ => token.toResult(json)


      }) match {
        case e: ReqlError => token.failure(e)
        case e: Exception => token.failure(ReqlClientError(e.getMessage, token.term))
        case e: Any => token.success(e)

        case null => token.failure(RethinkNoResultsError("No results found", token.term))

      }
    }
  }
}




