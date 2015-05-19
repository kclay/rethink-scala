package com.rethinkscala.net

import java.util.concurrent.TimeUnit

import com.google.common.cache.{Cache, CacheBuilder}
import com.rethinkscala.ResultExtractor
import com.rethinkscala.changefeeds.net.{ChangeCursor, ChangeCursorFactory}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 5/19/2015
 * Time: 8:04 AM 
 */
class ResultExtractorFactory {

  protected val cursorCache = CacheBuilder.newBuilder()
    .concurrencyLevel(4)
    .expireAfterAccess(10, TimeUnit.MINUTES)
    .build().asInstanceOf[Cache[Long, RethinkCursor[_]]]

  val changeCursorFactory = new ChangeCursorFactory(cursorCache)
  val defaultCursorFactor = new DefaultCursorFactory(cursorCache)
  private[rethinkscala] val changeCursorClass = classOf[ChangeCursor[_]]

  def create[T: Manifest] = {

    val mf = implicitly[Manifest[T]]
    val isChangeCursor = mf.runtimeClass isAssignableFrom changeCursorClass
    val factory = if (isChangeCursor) changeCursorFactory else defaultCursorFactor
    new ResultExtractor[T](factory, mf)
  }
}
