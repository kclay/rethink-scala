package com.rethinkscala.net

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/3/13
 * Time: 5:21 PM
 *
 */
case class DBResult(name: String, @JsonProperty("type") kind: String) extends Document
