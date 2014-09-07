package com.rethinkscala.net

import com.fasterxml.jackson.annotation._
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ArrayNode
import com.rethinkscala.{ResultExtractor,Term,Profile,Document,ConvertFrom}

import com.rethinkscala.ast.Datum
import com.rethinkscala.net.Translate._
import com.rethinkscala.reflect.Reflector
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
  val extractor:ResultExtractor[ResultType]


  def toError(response: R): RethinkError

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
    responseType, Seq(result.head.get(resultField).getOrElse(0) == 1),
    backtrace,
    profile)
}


case class ResultFolder[T](json: String)(implicit mf: Manifest[T]) {
  lazy val value = {
    val result = Reflector.fromJson(json)(mf)
    result match {
      case d: Document => {
        d.raw = json
        d
      }
      case _ => result
    }
  }
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
                                 @JsonProperty("p") profile: Option[Profile]) {

}


class JsonResponseExtractor {

  var result: Seq[String] = Seq.empty

  @JsonProperty("r")
  @JsonUnwrapped
  def apply(node: JsonNode) = {
    node match {
      case a: ArrayNode => result = (for (i <- 0 to a.size() - 1) yield a.get(i) match{
        case n:ArrayNode=> for(j <- 0 to n.size() - 1) yield n.get(j).toString
        case o:JsonNode=> Seq(o.toString)
      }  ).flatten
    }
  }

  def apply(index: Int) = result(index)
}

case class JsonResponse[T](@JsonProperty("t") responseType: Long,
                           @JsonProperty("r") result: Seq[T],
                           @JsonProperty("b") backtrace: Option[Seq[Frame]],
                           @JsonProperty("p") profile: Option[Profile]) extends BaseJsonResponse[T]

case class JsonQueryToken[R](connection: Connection, query: CompiledQuery, term: Term, p: Promise[R])(implicit val extractor: ResultExtractor[R])
  extends Token[String] with LazyLogging {


  type ResultType = R
  implicit lazy val mf = extractor.manifest

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


  def toCursor(id: Int, json: String, responseType: Int, atom: Boolean = false) = {


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

    val cursor = query.cursor(extractor.cursorFactory)(id, this, responseType match {
      case SUCCESS_SEQUENCE_VALUE => true
      case _ => false
    })

    cursor << seq2

  }

  def success(value: Any) = p.tryComplete(Try(value.asInstanceOf[R]))

  // TODO: Check performance and see if we can set the json value for type Document
  // as we parse it to avoid double parsing
  def raw(json: String) = Reflector.fromJson[JsonResponseExtractor](json)

  def cast[T](json: String)(implicit mf: Manifest[T]): T = {
    term match {
      case b: BinaryConversion =>
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


case class QueryToken[R](connection: Connection, query: CompiledQuery, term: Term, p: Promise[R])(implicit val extractor: ResultExtractor[R])
  extends Token[Response] with LazyLogging {


  implicit lazy val mf: Manifest[R] = extractor.manifest

  private[rethinkscala] def context = connection

  type ResultType = R
  type MapType = Map[String, _]
  type IterableType = Iterable[MapType]

  def cast[T](json: String)(implicit mf: Manifest[T]): T = translate[T].read(json, term)

  def toResult(response: Response) = {
    logger.debug(s"Processing result : $response")
    val json: String = Datum.unwrap(response.getResponse(0))

    val rtn = json match {
      case "" => None

      case _ => cast[ResultType](json)
    }
    rtn
  }


  def toError(response: Response) = ConvertFrom.toError(response, term)

  def success(value: Any) = p.tryComplete(Try(value.asInstanceOf[R]))

  def failure(e: Throwable) = p.tryFailure(e)

  def toCursor(id: Int, response: Response) = {

    import scala.collection.JavaConverters._
    //val seqManifest = implicitly[Manifest[Seq[R]]]


    val results = response.getResponseList.asScala
    val seq = results.length match {
      case 1 if response.getType == ResponseType.SUCCESS_ATOM =>
        cast[ResultType](Datum.unwrap(results(0)))
      case _ => for (d <- results) yield Datum.unwrap(d) match {

        case json: String => if (mf.typeArguments.nonEmpty) cast(json)(mf.typeArguments(0)) else cast[ResultType](json)
      }

    }
    val cursor = query.cursor(extractor.cursorFactory)(id, this, response.getType match {
      case ResponseType.SUCCESS_SEQUENCE => true
      case _ => false
    })

    cursor << seq.asInstanceOf[Seq[query.Result]]


  }


}