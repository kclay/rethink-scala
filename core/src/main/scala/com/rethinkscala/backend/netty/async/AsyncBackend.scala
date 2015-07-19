package com.rethinkscala.backend.netty.async

import com.rethinkscala.backend.RethinkBackend
import com.rethinkscala.net.RethinkError

import scala.concurrent.Future

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/15/2015
 * Time: 5:15 PM 
 */
object AsyncBackend extends RethinkBackend {
  override type ConnectionDef = AsyncConnection
  override type ProfileDef = AsyncProfile
  override type Creator = AsyncConnectionCreator
  override val profile: ProfileDef = AsyncProfile
  override val Connection: Creator = AsyncConnection

  type Result[T] =  Future[T]

}
