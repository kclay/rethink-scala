package com.rethinkscala.backend.netty.blocking

import com.rethinkscala.{ResultExtractor, Term}
import com.rethinkscala.net._

import scala.concurrent._
import scala.concurrent.duration.Duration

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/15/2015
 * Time: 4:28 PM 
 */

case class BlockingQuery[R](term: Term, connection: BlockingConnection, extractor: ResultExtractor[R],
                    opts: Map[String, Any], connectionId: Option[Long] = None)
  extends ResultQuery[R] with ResultResolver[Either[ReqlError, R]] {


  def toResult[T] = toResult(connection.timeoutDuration)

  def toResult[T](atMost: Duration): Either[ReqlError, R] = {

    val p = connection.write(term, opts, connectionId)(extractor)


    try {
      Await.ready(p.future, atMost)
    } catch {
      case e: TimeoutException => p failure ReqlTimeoutError(e.getMessage, term)
      case e: InterruptedException => p failure ReqlClientError(e.getMessage, term)


    }

    resolve(p.future.value)


  }

}
