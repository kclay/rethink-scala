package com.rethinkscala.net

import com.fasterxml.jackson.annotation.JsonProperty
import com.rethinkscala.reflect.Reflector
import scala.collection.mutable.ArrayBuffer
import com.rethinkscala.Schema


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


  private var _underlying: Map[String, Any] = Map.empty[String, Any]


  private[rethinkscala] def underlying(m: Map[String, Any]) = _underlying = m

  def \(name: String) = DocPath(_underlying, List(name))

  def toMap = _underlying

  private[rethinkscala] def invokeBeforeInsert = beforeInsert


  protected def beforeInsert = {}

  protected def afterInsert = {}

  protected def afterInsert(id: String) = {}

  private[rethinkscala] def invokeAfterInsert = afterInsert

  private[rethinkscala] def invokeAfterInsert(id: String) = afterInsert(id)
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


case class JoinResult[Left,Right](left:Left,right:Right) extends Document
abstract class InfoResult(name: String, @JsonProperty("type") kind: String) extends Document


case class InsertResult(inserted: Int = 0, replaced: Int = 0, unchanged: Int = 0, errors: Int = 0, @JsonProperty("first_error") firstError: Option[String] = None,

                        @JsonProperty("generated_keys") generatedKeys: Option[Seq[String]],
                        deleted: Int = 0, skipped: Int = 0) extends Document with ReturnValues with GeneratesKeys {

  //private var _returnValues = ???


}


case class TableInfoResult(name: String, @JsonProperty("type") kind: String, db: DBResult) extends InfoResult(name, kind)


case class ChangeResult(replaced: Int, unchanged: Int, inserted: Int, deleted: Int, errors: Int, @JsonProperty("first_error") firstError: Option[String],
                        skipped: Int, @JsonProperty("generated_keys") generatedKeys: Option[Seq[String]]) extends Document with ReturnValues with GeneratesKeys