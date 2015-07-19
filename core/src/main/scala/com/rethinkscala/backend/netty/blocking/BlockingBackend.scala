package com.rethinkscala.backend.netty.blocking

import com.rethinkscala.backend.RethinkBackend
import com.rethinkscala.net.RethinkError

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/15/2015
 * Time: 4:41 PM 
 */
object BlockingBackend extends RethinkBackend {

  override type ConnectionDef = BlockingConnection
  override type Creator = BlockingConnectionCreator

  override type ProfileDef = BlockingProfile
  override val profile = BlockingProfile
  override val Connection = BlockingConnection
  type Result[T] = Either[RethinkError, T]
}


