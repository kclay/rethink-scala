package com.rethinkscala

import com.fasterxml.jackson.annotation.{JsonIgnore, JsonProperty}
import com.rethinkscala.reflect.Reflector
import com.rethinkscala.net.WithConversion


case class DocPath(root: Map[String, Any], paths: List[String]) {

  type M = Map[String, Any]

  def as[T] = find[T](root)

  private def find[T](value: Any, p: List[String] = paths): Option[T] = p.headOption.map {
    x => value match {

      case Some(x) => find[T](x, p)
      case m: M => find[T](m.get(x), p.tail)
      case _ => None
    }
  }.getOrElse(value match {

    case Some(x) => Some(x.asInstanceOf[T])
    case x: Any => Some(x.asInstanceOf[T])
    case _ => None
  })

  def \(name: String) = DocPath(root, paths :+ name)
}

trait Document {


  @JsonIgnore
  private[rethinkscala] lazy val underlying: Map[String, Any] = Reflector.fromJson[Map[String, Any]](raw)

  private[rethinkscala] var raw: String = _


  def \(name: String) = DocPath(underlying, List(name))

  def toMap = underlying

  //def toJson = _raw

  private[rethinkscala] def invokeBeforeInsert = beforeInsert


  protected def beforeInsert = {}

  protected def afterInsert = {}

  protected def afterInsert(id: String) = {}

  private[rethinkscala] def invokeAfterInsert = afterInsert

  private[rethinkscala] def invokeAfterInsert(id: String) = afterInsert(id)
}


class JsonDocument(json: String) {
  private[rethinkscala] lazy val underlying: Either[Map[String, Any], Seq[Any]] = if (raw.startsWith("{"))
    Left(Reflector.fromJson[Map[String, Any]](raw))
  else
    Right(Reflector.fromJson[Seq[Any]](raw))


  private[rethinkscala] var raw: String = json

  def \(name: String) = DocPath(toMap, List(name))

  def toMap = underlying fold(a => a, b => (b.zipWithIndex map {
    case (value: Any, index: Int) => (index.toString, value)

  }).toMap)

  def asRaw = raw

  def toInt = Integer.parseInt(raw, 10)


  def toList = underlying.fold(_.toList, _.toList)
}

trait KeyedDocument[K] extends Document {
  type Key = K
  val id: Key
}

trait GeneratesKeys {
  val generatedKeys: Option[Seq[String]]
}

trait ReturnValues {
  self: Document =>
  private var _returnedValue: Option[Any] = None

  def returnedValue[T](implicit mf: Manifest[T]): Option[T] = {

    if (_returnedValue.isEmpty) {
      _returnedValue = (this \ "new_val").as[Map[String, Any]] match {
        case Some(s) => Some(Reflector.fromJson[T](Reflector.toJson(s)))
        case _ => None
      }
    }
    _returnedValue.asInstanceOf[Option[T]]
  }

}

case class DBResult(name: String, @JsonProperty("type") kind: String) extends Document


case class JoinResult[Left, Right](left: Left, right: Right) extends Document

class ZipResult[L, R] extends Document {


  private var _left: Option[L] = None
  private var _right: Option[R] = None

  def left(implicit mf: Manifest[L]): L = _left.getOrElse {
    val value = Reflector.fromJson[L](raw)
    _left = Some(value)
    value

  }

  def right(implicit mf: Manifest[R]): R = _right.getOrElse {
    val value = Reflector.fromJson[R](raw)
    _right = Some(value)
    value

  }
}

abstract class InfoResult(name: String, @JsonProperty("type") kind: String) extends Document


case class InsertResult(inserted: Int = 0, replaced: Int = 0, unchanged: Int = 0, errors: Int = 0, @JsonProperty("first_error") firstError: Option[String] = None,

                        @JsonProperty("generated_keys") generatedKeys: Option[Seq[String]],
                        deleted: Int = 0, skipped: Int = 0) extends Document with ReturnValues with GeneratesKeys {

  //private var _returnValues = ???


}

case class IndexStatusResult(index: String, ready: Boolean) extends Document

case class TableInfoResult(name: String, @JsonProperty("type") kind: String, db: DBResult) extends InfoResult(name, kind)


case class ChangeResult(replaced: Int, unchanged: Int, inserted: Int, deleted: Int, errors: Int, @JsonProperty("first_error") firstError: Option[String],
                        skipped: Int, @JsonProperty("generated_keys") generatedKeys: Option[Seq[String]]) extends Document
with ReturnValues
with GeneratesKeys

case class MatchResult(start: Int, end: Int, str: String, groups: Seq[MatchGroupResult]) extends Document

case class MatchGroupResult(start: Int, end: Int, str: String)

case class Profile(description: String, @JsonProperty("duration(ms)") duration: Double, @JsonProperty("sub_task") subTask: List[Profile], parallelTasks: Option[Profile])

case class QueryProfile[T](value: T, profile: Profile)