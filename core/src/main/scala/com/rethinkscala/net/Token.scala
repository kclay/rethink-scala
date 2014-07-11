package com.rethinkscala.net

import com.fasterxml.jackson.annotation.JsonProperty
import com.rethinkscala.ConvertFrom._
import com.rethinkscala.{Profile, Term}
import com.rethinkscala.ast.{ProduceSequence, Datum}
import com.rethinkscala.net.Translate._
import com.typesafe.scalalogging.slf4j.LazyLogging
import ql2.Ql2.Response
import ql2.Ql2.Response.ResponseType

import scala.concurrent.Promise

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
  val connection:Connection

  def handle(response: R)

  def failure(e: Throwable)

}

case class JsonResponse(@JsonProperty("t")responseType:Long,
                        @JsonProperty("r")json:String,
                        @JsonProperty("b")backtrace:Option[Frame],
                        @JsonProperty("p")profile:Option[Profile])
case class JsonQueryToken[R](connection:Connection,query:CompiledQuery,term:Term,p:Promise[R],mf:Manifest[R]) extends Token[JsonResponse] with LazyLogging{
  override type ResultType = R

  override def failure(e: Throwable) =  p failure e

  override def handle(response: JsonResponse) = ???

  def cast[T](json: String)(implicit mf: Manifest[T]): T = translate[T].read(json, term)
  def toResult(response: JsonResponse) = {
    logger.debug(s"Processing result : $response")


    val rtn = response.json match {
      case "" => None

      case _ => cast[ResultType](response.json)(mf)
    }
    rtn
  }
}




case class QueryToken[R](connection: Connection, query: CompiledQuery, term: Term, p: Promise[R], mf: Manifest[R])
  extends Token[Response] with LazyLogging {

  implicit val t = mf

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

  def handle(response: Response) = (response.getType match {

    case ResponseType.RUNTIME_ERROR | ResponseType.COMPILE_ERROR | ResponseType.CLIENT_ERROR => toError(response, term)
    case ResponseType.SUCCESS_PARTIAL | ResponseType.SUCCESS_SEQUENCE => toCursor(0, response)
    case ResponseType.SUCCESS_ATOM => term match {
      case x: ProduceSequence[_] => toCursor(0, response)
      case _ => toResult(response)
    }
    // case ResponseType.SUCCESS_ATOM => toResult(response)
    case _ =>

  }) match {
    case e: Exception => p failure e
    case e: Any => p success e.asInstanceOf[R]
  }

  def failure(e: Throwable) = p failure e


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

    new Cursor[R](id, this, seq.asInstanceOf[Seq[R]], response.getType match {
      case ResponseType.SUCCESS_SEQUENCE => true
      case _ => false
    })

  }


}