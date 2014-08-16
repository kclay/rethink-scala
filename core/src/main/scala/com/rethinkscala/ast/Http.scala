package com.rethinkscala.ast


import java.io.File

import com.rethinkscala.HttpOptions
import ql2.Ql2.Term.TermType

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/19/14
 * Time: 12:23 PM
 *
 */
case class Http(url: String, options: HttpOptions) extends ProduceAnyDocument {

  override lazy val args = buildArgs(url)
  override lazy val optargs = buildOptArgs(options.toMap)

  override def termType = TermType.HTTP
}


object ResultFormat extends Enumeration {
  type Kind = Value
  val Text = Value("text")
  val Json = Value("json")
  val Jsonp = Value("jsonp")
  val Auto = Value("auto")
}

object HttpMethod extends Enumeration {
  type Kind = Value
  val GET = Value("GET")
  val POST = Value("POST")
  val PUT = Value("PUT")
  val PATCH = Value("PATCH")
  val DELETE = Value("DELETE")
  val HEAD = Value("HEAD")


}

sealed abstract class HttpAuth {
  val user: String
  val pass: String
  val `type`: String
}

case class BasicAuth(user: String, pass: String) extends HttpAuth {
  override val `type`: String = "basic"
}

case class DigestAuth(user: String, pass: String) extends HttpAuth {
  override val `type`: String = "digest"
}

object HttpHeader {
  implicit def tuple2ToHttpHeader(t: (String, String)) = HttpHeader(t._1, t._2)
}

object HttpHeaders {
  implicit def iterableTupleToHttpHeaders(i: Seq[(String, String)]) = HttpHeaders(i.map(h => HttpHeader(h._1, h._2)))

  def empty = HttpHeaders(List.empty)
}

case class HttpHeaders(headers: Seq[HttpHeader]) {
  def ++(other: HttpHeaders) = copy(headers = headers ++ other.headers)

  def ++(other: Seq[HttpHeader]) = copy(headers = headers ++ other)
}

case class HttpHeader(key: String, value: String)


object HttpBody {
  implicit def stringToHttpBody(value: String) = HttpStringBody(value)

  implicit def mapToHttpFormBody(value: Map[String, Any]) = HttpFormBody(value)

  implicit def fileToHttpFormBody(value: File) = HttpFileBody(value)
}

trait HttpBody[T] {
  val value: T
}

case class HttpStringBody(value: String) extends HttpBody[String]

case class HttpFormBody(value: Map[String, Any]) extends HttpBody[Map[String, Any]]

case class HttpFileBody(file: File) extends HttpBody[String] {

  private[this] lazy val contents = {
    val source = scala.io.Source.fromFile(file)
    try {
      source.getLines() mkString "\n"
    } finally {
      source.close()
    }
  }
  lazy val value: String = contents
}

trait PaginationStrategy {
  type T
  val value: T
}

object LinkNext extends PaginationStrategy {
  type T = String
  override val value = "link-next"
}

object PaginationStrategy {

  implicit def funcToPredicte(f: PaginationData => Typed) = new PaginationPredicate(f)

  implicit def predicateToStrategy(f: PaginationPredicate) = FunctionPaginationStrategy(f)

  def apply(f: PaginationData => Typed) = FunctionPaginationStrategy(f)

}

object PaginationData {
  implicit def varToPaginationData(v: Var) = new PaginationData(v)
}

class PaginationData(v: Var) {
  lazy val params = v \ "params"
  lazy val header = v \ "header"
  lazy val value = v \ "value"
}

class PaginationPredicate(f: PaginationData => Typed) extends Predicate1 {
  protected def _invoke(vars: Seq[Var]) = f(vars(0))

  val amount: Int = 1
}

case class FunctionPaginationStrategy(value: PaginationPredicate) extends PaginationStrategy {
  type T = PaginationPredicate

}