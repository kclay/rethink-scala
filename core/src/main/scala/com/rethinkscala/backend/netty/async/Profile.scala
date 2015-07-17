package com.rethinkscala.backend.netty.async

import com.rethinkscala.Implicits.Common
import com.rethinkscala.ResultExtractor
import com.rethinkscala.ast.Produce
import com.rethinkscala.backend.QueryMode
import com.rethinkscala.net.Version

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/15/2015
 * Time: 5:19 PM 
 */

trait LowPriorityCommon {
  implicit def toAsyncResultExtractor[T: Manifest](implicit connection: AsyncConnection): ResultExtractor[T] = connection
    .resultExtractorFactory
    .create[T]
}


trait AsyncQueryMode extends QueryMode[AsyncConnection] {
  def apply(version: Version) = AsyncConnection(version)

  implicit def toDelegate[T](produce: Produce[T])(implicit connection: AsyncConnection): AsyncDelegate[T] = new AsyncDelegate[T](produce, connection)

  type DelegateDef[T] = AsyncDelegate[T]
  type Producer[T] = Produce[T]


  override def apply[T](produce: Produce[T])(implicit connection: AsyncConnection): AsyncDelegate[T] = toDelegate(produce)
}


trait AsyncProfile extends LowPriorityCommon with AsyncQueryMode with Common


object AsyncProfile extends AsyncProfile