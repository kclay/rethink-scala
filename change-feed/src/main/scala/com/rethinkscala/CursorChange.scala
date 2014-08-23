package com.rethinkscala

import com.rethinkscala.net.{ChangeCursorFactory, DefaultCursorFactory}

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 9:07 PM
 *
 */

object CursorChange{
  implicit def toResultExtractor[T:Manifest] = new ResultExtractor[T](ChangeCursorFactory,implicitly[Manifest[T]])
}
case class CursorChange[T](old:Option[T],current:Option[T]) extends Document

