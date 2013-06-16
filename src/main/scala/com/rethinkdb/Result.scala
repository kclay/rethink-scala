package com.rethinkdb

import scala.annotation.StaticAnnotation
import com.fasterxml.jackson.annotation.JsonProperty



case class Field(name:String) extends StaticAnnotation

trait  Document

case class InsertResult(inserted: Int = 0, replaced: Int = 0, unchanged: Int = 0, errors: Int = 0, firstError: Option[String] = None, generatedKeys: Seq[Any] = Seq.empty[Any],
                        deleted: Int = 0, skipped: Int = 0) extends Document

abstract class InfoResult(name: String, @JsonProperty("type")kind: String) extends Document

case class DBResult(name:String,@JsonProperty("type")kind:String) extends Document
case class TableInfoResult(name: String, @JsonProperty("type")kind: String,db:DBResult) extends Document
