package com

import com.rethinkscala.changefeeds.net.{ChangeCursor, ChangeCursorFactory}
import com.rethinkscala.net._


/** Created by IntelliJ IDEA.
  * User: Keyston
  * Date: 3/19/13
  * Time: 7:32 PM
  */
package object rethinkscala extends ImplicitConversions with Helpers {


  private[rethinkscala] trait FilterTyped

  private[rethinkscala] val changeCursorClass = classOf[ChangeCursor[_]]

  implicit def toResultExtractor[T: Manifest]:ResultExtractor[T] = {

    val mf = implicitly[Manifest[T]]
    val isChangeCursor = mf.runtimeClass isAssignableFrom changeCursorClass
    val factory = if (isChangeCursor) ChangeCursorFactory else DefaultCursorFactory
    new ResultExtractor[T](factory, mf)
  }


}
