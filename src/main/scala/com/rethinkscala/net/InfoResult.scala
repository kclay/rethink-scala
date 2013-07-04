package com.rethinkscala.net

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/3/13
 * Time: 5:21 PM
 *
 */
abstract class InfoResult(name: String, @JsonProperty("type") kind: String) extends Document
