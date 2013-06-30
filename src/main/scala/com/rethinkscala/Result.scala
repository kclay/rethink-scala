package com.rethinkscala

import scala.annotation.StaticAnnotation
import com.fasterxml.jackson.annotation.JsonProperty

trait Document

trait KeyedDocument extends Document

case class InsertResult(inserted: Int = 0, replaced: Int = 0, unchanged: Int = 0, errors: Int = 0, @JsonProperty("first_error") firstError: Option[String] = None,

                        @JsonProperty("generated_keys") generatedKeys: Seq[String] = Seq.empty[String],
                        deleted: Int = 0, skipped: Int = 0) extends Document

abstract class InfoResult(name: String, @JsonProperty("type") kind: String) extends Document

case class DBResult(name: String, @JsonProperty("type") kind: String) extends Document
case class TableInfoResult(name: String, @JsonProperty("type") kind: String, db: DBResult) extends InfoResult(name, kind)

case class ReplaceResult(replaced: Int, unchanged: Int, inserted: Int, deleted: Int, errors: Int, @JsonProperty("first_error") firstError: Option[String], skipped: Int) extends Document
case class UpdateResult(replaced: Int, unchanged: Int, skipped: Int, errors: Int,
                        @JsonProperty("first_error") firstError: Option[String], deleted: Int, inserted: Int) extends Document
