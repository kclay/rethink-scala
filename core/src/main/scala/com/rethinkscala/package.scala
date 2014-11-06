package com

import com.rethinkscala.ast._
import com.rethinkscala.net._

import scala.concurrent.Future


/** Created by IntelliJ IDEA.
  * User: Keyston
  * Date: 3/19/13
  * Time: 7:32 PM
  */
package object rethinkscala extends ImplicitConversions with Helpers{


  private[rethinkscala] trait FilterTyped
  implicit def toResultExtractor[T: Manifest] = new ResultExtractor[T](DefaultCursorFactory, implicitly[Manifest[T]])



}
