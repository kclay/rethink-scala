package com.rethinkscala.net

import com.fasterxml.jackson.annotation.JsonProperty
import com.rethinkscala.ConvertFrom._
import com.rethinkscala.reflect.Reflector
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




case class JsonBinaryResponse(@JsonProperty("t")responseType:Long,
                                @JsonProperty("r")result:Seq[Map[String,Int]],
                                @JsonProperty("b")backtrace:Option[Seq[Frame]],
                                @JsonProperty("p")profile:Option[Profile]){
  def convert(resultField:String) = JsonResponse(
        responseType,result.head.get(resultField).getOrElse(0) == 1,
  backtrace,
  profile)
}

case class JsonErrorResponse(
                              @JsonProperty("t")responseType:Long,
                              @JsonProperty("r")result:Seq[String],
                              @JsonProperty("b")backtrace:Option[Seq[Frame]],
                              @JsonProperty("p")profile:Option[Profile]
                              )
case class JsonResponse[T](@JsonProperty("t")responseType:Long,
                        @JsonProperty("r")result:T,
                        @JsonProperty("b")backtrace:Option[Seq[Frame]],
                        @JsonProperty("p")profile:Option[Profile])
case class JsonQueryToken[R](connection:Connection,query:CompiledQuery,term:Term,p:Promise[R])(implicit mf:Manifest[R]) extends Token[String] with LazyLogging{
  override type ResultType = R


  import ResponseType.{RUNTIME_ERROR_VALUE,COMPILE_ERROR_VALUE,
  CLIENT_ERROR_VALUE,SUCCESS_PARTIAL_VALUE,SUCCESS_SEQUENCE_VALUE,SUCCESS_ATOM_VALUE }
    val ResponseTypeExtractor = """"t":(\d+)""".r.unanchored

  override def failure(e: Throwable) =  p failure e


  def toError(json:String) = {
    val response =Reflector.fromJson[JsonErrorResponse](json)
    val error = response.result.head
    val frames = response.backtrace.getOrElse(Iterable.empty)



    response.responseType match{
      case RUNTIME_ERROR_VALUE=>RethinkRuntimeError(error,term,frames)
      case COMPILE_ERROR_VALUE=>RethinkCompileError(error,term,frames)
      case CLIENT_ERROR_VALUE=> RethinkClientError(error,term,frames)
    }
  }
  override def handle(json: String) =( json match{
    case ResponseTypeExtractor(responseType) => responseType.toInt match{
      case RUNTIME_ERROR_VALUE | COMPILE_ERROR_VALUE|CLIENT_ERROR_VALUE=>toError(json)
      case SUCCESS_PARTIAL_VALUE | SUCCESS_SEQUENCE_VALUE => toCursor(0, json,responseType.toInt)
      case SUCCESS_ATOM_VALUE=> term match {
        case x: ProduceSequence[_] => toCursor(0, json,responseType.toInt)
        case _ => toResult(json)
      }
      case _=> RethinkRuntimeError(s"Invalid response = $json",term)
    }
  } ) match {
    case e: Exception => p failure e
    case e: Any => p success e.asInstanceOf[R]
  }

  def toCursor(id:Int,json:String,responseType:Int)={
    //val seqMan = Manifest.classType(classOf[Seq[R]],mf)
    val manifest = implicitly[Manifest[JsonResponse[Seq[R]]]]
    val seq = cast(json)(manifest).result
    new Cursor[R](id, this, seq, responseType match {
      case SUCCESS_SEQUENCE_VALUE => true
      case _ => false
    })

  }

  def cast[T](json: String)(implicit mf: Manifest[T]): T =  {
    term match {
      case b:BinaryConversion =>
        Reflector.fromJson[JsonBinaryResponse](json)
        .convert(b.resultField).asInstanceOf[T]
      case _=>translate[T].read(json, term)
    }
    
  }


  def toResult(json:String) = {
    val manifest = implicitly[Manifest[JsonResponse[R]]]
    cast(json)(manifest).result
  }
}




case class QueryToken[R](connection: Connection, query: CompiledQuery, term: Term, p: Promise[R])(implicit mf: Manifest[R])
  extends Token[Response] with LazyLogging {



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