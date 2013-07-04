package com.rethinkscala.net

import scala.annotation.StaticAnnotation
import com.fasterxml.jackson.annotation.JsonProperty














case class ChangeResult(replaced: Int, unchanged: Int, inserted: Int, deleted: Int, errors: Int, @JsonProperty("first_error") firstError: Option[String], skipped: Int) extends Document

