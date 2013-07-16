package com.rethinkscala.net

import com.fasterxml.jackson.annotation.JsonProperty
import com.rethinkscala.reflect.Reflector


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
}

trait KeyedDocument[K] extends Document {
  type Key = K
  val id: Key
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

abstract class InfoResult(name: String, @JsonProperty("type") kind: String) extends Document


case class InsertResult(inserted: Int = 0, replaced: Int = 0, unchanged: Int = 0, errors: Int = 0, @JsonProperty("first_error") firstError: Option[String] = None,

                        @JsonProperty("generated_keys") generatedKeys: Seq[String] = Seq.empty[String],
                        deleted: Int = 0, skipped: Int = 0) extends Document with ReturnValues {

  //private var _returnValues = ???


}


case class TableInfoResult(name: String, @JsonProperty("type") kind: String, db: DBResult) extends InfoResult(name, kind)


case class ChangeResult(replaced: Int, unchanged: Int, inserted: Int, deleted: Int, errors: Int, @JsonProperty("first_error") firstError: Option[String], skipped: Int) extends Document