package com.rethinkscala.net

import java.util.concurrent.TimeUnit

import com.google.common.cache.{Cache, CacheBuilder}
import com.rethinkscala.{CursorChange, ResultExtractor}
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


    // For change feeds we need to make sure we pass an RethinkCursor[CursorChange[T]]
    // by default the passed manifest will be ChangeCursor[T]
    // this will cause the jackson deseralizer to fail and return an empty/default instance for the change
    // Since we have all the type information needed we can construct the correct manifest at runtime and
    // still provide type safety
    val isChangeCursor = mf.runtimeClass isAssignableFrom changeCursorClass
    val manifest = if (isChangeCursor) {
      Manifest.classType(classOf[RethinkCursor[_]],
        Manifest.classType(classOf[CursorChange[_]], mf.typeArguments.head))
        .asInstanceOf[Manifest[T]]
    } else mf
    val factory = if (isChangeCursor) changeCursorFactory else defaultCursorFactor
    new ResultExtractor[T](factory, manifest)
  }
}
