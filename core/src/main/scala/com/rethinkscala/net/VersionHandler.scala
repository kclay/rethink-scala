package com.rethinkscala.net

import java.util.concurrent.atomic.AtomicLong

import com.rethinkscala.Term
import com.rethinkscala.ast.ProduceSequence
import ql2.Ql2.Response.ResponseType._

import scala.concurrent.Promise
import scala.util.matching.Regex

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 8/22/2014
 * Time: 2:22 PM 
 */
trait VersionHandler[R] {
  val version: Version

  private val newTokenId: AtomicLong = new AtomicLong()

  def handle(tokenId: Long, response: R)

  type TokenType <: Token[_]


  private[rethinkscala] val tokensById = new com.google.common.collect.MapMaker()
    .concurrencyLevel(4)
    .weakKeys()
    .makeMap[Long, TokenType]

  def newQuery[T](conn: Connection, term: Term, promise: Promise[T],
                  db: Option[String] = None, opts: Map[String, Any] = Map())(implicit mf: Manifest[T]): CompiledQuery[T] = {
    val tokenId = newTokenId.getAndIncrement
    val query = version.toQuery[T](term, newTokenId.getAndIncrement, db, opts)
    val token = query.asToken(conn, term, promise)
    tokensById.putIfAbsent(tokenId, token.asInstanceOf[TokenType])
    query
  }

  def handle[T](id: Long)(f: TokenType => Unit) = {
    Option(tokensById.get(id)).map(f)
  }


}

case class JsonVersionHandler(version: Version3) extends VersionHandler[String] {

  type TokenType = JsonQueryToken[_]
  val ResponseTypeExtractor = """"t":(\d+)""".r.unanchored

  override def handle(tokenId: Long, json: String) = handle(tokenId) {
    token => (json match {
      case ResponseTypeExtractor(responseType) => responseType.toInt match {
        case RUNTIME_ERROR_VALUE | COMPILE_ERROR_VALUE | CLIENT_ERROR_VALUE => token.toError(json)
        case SUCCESS_PARTIAL_VALUE | SUCCESS_SEQUENCE_VALUE => token.toCursor(0, json, responseType.toInt)
        case SUCCESS_ATOM_VALUE => token.term match {
          case x: ProduceSequence[_] => token.toCursor(0, json, responseType.toInt, atom = true)
          case _ => token.toResult(json)
        }
        case _ => RethinkRuntimeError(s"Invalid response = $json", token.term)
      }
    }) match {
      case e: Exception => token.failure(e)
      case e: Any => token.success(e)
    }
  }
}
