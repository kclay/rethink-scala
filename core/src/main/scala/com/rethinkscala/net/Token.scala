package com.rethinkscala.net

import com.fasterxml.jackson.annotation._
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.{ArrayNode, ObjectNode}
import com.rethinkscala.ast.Datum
import com.rethinkscala.changefeeds.net.ChangeCursor
import com.rethinkscala.net.Translate._
import com.rethinkscala.reflect.Reflector
import com.rethinkscala.{ConvertFrom, Document, Profile, ResultExtractor, Term}
import com.typesafe.scalalogging.slf4j.LazyLogging
import ql2.Ql2.Response
import ql2.Ql2.Response.ResponseType

import scala.concurrent.Promise
import scala.util.Try

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/6/14
 * Time: 3:37 PM
 *
 */


abstract class Token[R] {
  type ResultType

  val query: CompiledQuery

  val term: Term

  val connection: Connection

  val extractor: ResultExtractor[ResultType]

  def id: Long

  def toError(response: R): ReqlError

  def failure(e: Throwable)

  def success(e: Any)

}


trait BaseJsonResponse[T] {

  val responseType: Long
  val result: Seq[T]
  val backtrace: Option[Seq[Frame]]
  val profile: Option[Profile]
  lazy val single = result.head
}

case class JsonBinaryResponse(@JsonProperty("t") responseType: Long,
                              @JsonProperty("r") result: Seq[Map[String, Int]],
                              @JsonProperty("b") backtrace: Option[Seq[Frame]],
                              @JsonProperty("p") profile: Option[Profile]) extends BaseJsonResponse[Map[String, Int]] {
  def convert(resultField: String) = JsonResponse(
    responseType, Seq(result.head.getOrElse(resultField, 0) == 1),
    backtrace,
    profile)
}


case class JsonErrorResponse(
                              @JsonProperty("t") responseType: Long,
                              @JsonProperty("r") result: Seq[String],
                              @JsonProperty("b") backtrace: Option[Seq[Frame]],
                              @JsonProperty("p") profile: Option[Profile]
                              ) extends BaseJsonResponse[String]

case class JsonCursorResponse[T](@JsonProperty("t") responseType: Long,
                                 @JsonProperty("r") result: T,
                                 @JsonProperty("b") backtrace: Option[Seq[Frame]],
                                 @JsonProperty("p") profile: Option[Profile])


class JsonResponseExtractor {

  var result: Seq[String] = Seq.empty

  @JsonProperty("r")
  @JsonUnwrapped
  def apply(node: JsonNode) = {
    node match {
      case a: ArrayNode => result = (for (i <- 0 to a.size() - 1) yield a.get(i) match {
        case o: ObjectNode if o.get("$reql_type$") != null => {
          Try({


            val entries = o.get("data").asInstanceOf[ArrayNode]
            for (i <- 0 to entries.size() - 1) yield entries.get(i).toString
          }).getOrElse(Seq.empty)


        }
        case n: ArrayNode => for (j <- 0 to n.size() - 1) yield n.get(j).toString
        case o: JsonNode => Seq(o.toString)
      }).flatten
    }
  }

  def apply(index: Int) = result(index)
}

case class JsonResponse[T](@JsonProperty("t") responseType: Long,
                           @JsonProperty("r") result: Seq[T],
                           @JsonProperty("b") backtrace: Option[Seq[Frame]],
                           @JsonProperty("p") profile: Option[Profile]) extends BaseJsonResponse[T]

case class JsonQueryToken[R](connection: Connection, connectionId: Long, query: CompiledQuery,
                             term: Term, p: Promise[R])(implicit val extractor: ResultExtractor[R])
  extends Token[String] with LazyLogging {


  type ResultType = R
  implicit lazy val mf = extractor.manifest

  def id: Long = query.tokenId

  import ql2.Ql2.Response.ResponseType.{CLIENT_ERROR_VALUE, COMPILE_ERROR_VALUE, RUNTIME_ERROR_VALUE, SUCCESS_SEQUENCE_VALUE}

  val ResponseTypeExtractor = """"t":(\d+)""".r.unanchored

  override def failure(e: Throwable) = p.tryFailure(e)


  def toError(json: String) = {
    val response = Reflector.fromJson[JsonErrorResponse](json)
    val error = response.result.head
    val frames = response.backtrace.getOrElse(Iterable.empty)

    response.responseType match {
      case RUNTIME_ERROR_VALUE => RethinkRuntimeError(error, term, frames)
      case COMPILE_ERROR_VALUE => RethinkCompileError(error, term, frames)
      case CLIENT_ERROR_VALUE => RethinkClientError(error, term, frames)
    }
  }


  val manifest = implicitly[Manifest[JsonResponse[R]]]
  val cursorManifest = implicitly[Manifest[JsonCursorResponse[R]]]


  def toCursor(json: String, responseType: Int, atom: Boolean = false) = {

    if (json.contains("new_val")) {
      val b = json
    }
    val seq = (atom match {
      case true => cast(json)(manifest).single
      case _ => cast(json)(cursorManifest).result
    }).asInstanceOf[Seq[query.Result]]

    val seq2 = seq.headOption match {
      case Some(d: Document) => {
        val rawJson = raw(json)
        seq.zipWithIndex.collect {
          case (d: Document, index) => d.raw = rawJson(index); d
        }
      }.asInstanceOf[Seq[query.Result]]
      case _ => seq
    }

    val cursor = query.cursor(this, connectionId, responseType match {
      case SUCCESS_SEQUENCE_VALUE => true
      case _ => false
    })(extractor.cursorFactory)


    cursor << seq2.asInstanceOf[Seq[cursor.ChunkType]]

  }

  def success(value: Any) = p.tryComplete(Try(value.asInstanceOf[R]))

  // TODO: Check performance and see if we can set the json value for type Document
  // as we parse it to avoid double parsing
  def raw(json: String) = Reflector.fromJson[JsonResponseExtractor](json)

  def cast[T](json: String)(implicit mf: Manifest[T]): T = {
    term match {
      case b: BinaryConversion if mf.typeArguments.headOption.exists(_.runtimeClass == classOf[Boolean]) =>
        Reflector.fromJson[JsonBinaryResponse](json)
          .convert(b.resultField).asInstanceOf[T]
      case _ => Reflector.fromJson(json)(mf)
    }

  }


  def toResult(json: String) = {
    val result = cast(json)(manifest).single
    result match {
      case d: Document => d.raw = raw(json)(0); d.asInstanceOf[R]
      case _ => result
    }
  }
}

