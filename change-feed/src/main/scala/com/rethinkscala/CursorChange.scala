package com.rethinkscala

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 9:07 PM
 *
 */
case class CursorChange[T](old:Option[T],current:Option[T]) extends Document

