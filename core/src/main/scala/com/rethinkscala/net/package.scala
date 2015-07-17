package com.rethinkscala

import com.rethinkscala.backend.netty.async.AsyncBackend
import com.rethinkscala.backend.netty.blocking.BlockingBackend

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/16/2015
 * Time: 8:51 AM 
 */
package object net {

  @deprecated("import com.rethinkscala.Blocking._", "0.4.8")
  val Blocking = BlockingBackend.profile
  @deprecated("import com.rethinkscala.Async._", "0.4.8")
  val Async = AsyncBackend.profile
}
