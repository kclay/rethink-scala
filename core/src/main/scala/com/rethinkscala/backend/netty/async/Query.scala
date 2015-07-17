package com.rethinkscala.backend.netty.async

import com.rethinkscala.net.{ResultQuery, ResultResolver}
import com.rethinkscala.{ResultExtractor, Term}

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/15/2015
 * Time: 5:43 PM 
 */
case class AsyncQuery[R](term: Term, connection: AsyncConnection, extractor: ResultExtractor[R],
                               opts: Map[String, Any], connectionId: Option[Long] = None)
  extends ResultQuery[R] with ResultResolver[Future[R]] {


  implicit val exc: ExecutionContext = connection.version.executionContext

  def toResult[T] = run[T]

  protected def run[T] = {


    val p = connection.write(term, opts, connectionId)(extractor)
    // FIXME : Write this better
    p.future.transform(t => resolve(t).right.get, e => resolve(e).left.get)


  }
}
