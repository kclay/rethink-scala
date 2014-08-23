package com.rethinkscala.japi

;

import com.rethinkscala.net.{DefaultCursorFactory, RethinkError, BlockingConnection, Version}
import com.rethinkscala.ast.Produce
import scala.concurrent.duration.Duration
import com.rethinkscala.{ResultExtractor, Delegate}
import java.lang.reflect.ParameterizedType
import java.util.{Map => JMap, List => JList}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/2/14
 * Time: 8:37 PM 
 */

case class Connection(version: Version, timeoutInMilliseconds: Long = 5000) {


  private[this] val underlying = BlockingConnection(version, Duration(timeoutInMilliseconds / 1000, "seconds"))

  type JavaMap[V] = JMap[String, V]
  type JavaSeq[E] = JList[E]




  def run[T <: AnyRef](producer: Produce[T]): Result[T] = {
    val clazz = {
      val t = producer.getClass.getGenericSuperclass.asInstanceOf[ParameterizedType]
      t.getActualTypeArguments()(0).asInstanceOf[Class[_]]
    }
    val extractor = ResultExtractor[T](DefaultCursorFactory,Manifest.classType(clazz))
    QueryResult(Delegate(producer, underlying).run(extractor)).asInstanceOf[Result[T]]
  }


}


private case class QueryResult[T <: AnyRef](underlying: Either[RethinkError, T]) extends Result[T] {
  def hasError = underlying.isLeft

  def getError = underlying.left.get

  def hasResults = underlying.isRight

  def getResults: T = underlying.right.get
}

trait Result[T <: AnyRef] {

  def hasError: Boolean

  def getError: RethinkError

  def hasResults: Boolean

  def getResults: T
}


