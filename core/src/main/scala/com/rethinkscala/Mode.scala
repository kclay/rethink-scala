package com.rethinkscala

import com.rethinkscala.ast._
import com.rethinkscala.net._
import scala.concurrent.Future
import com.rethinkscala.net.BlockingConnection
import com.rethinkscala.net.AsyncConnection
import scala.Some

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 8/3/13
 * Time: 5:54 PM 
 */


trait Async {

  implicit class ProduceAsync[T](p: Produce[T]) {
    private def cast[R](implicit c: Connection, mf: Manifest[R]): Future[R] = p.toQuery[R].asInstanceOf[AsyncQuery[R]].toResult

    def run(implicit c: Connection, mf: Manifest[T]) = cast[T]


    def as[R <: T](implicit c: Connection, tt: Manifest[R]) = cast[R]


  }


  implicit class ProduceAsyncSeq[T](p: ProduceSequence[T])(implicit mf: Manifest[T]) {

    private def cast[R](implicit c: Connection, mf: Manifest[R]): Future[Seq[R]] = p.toQuery[R].asInstanceOf[AsyncQuery[Seq[R]]].toResult

    def run(implicit c: Connection, mf: Manifest[T], d: DummyImplicit) = cast[T]


    def as[R <: T](implicit c: Connection, mf: Manifest[R]) = cast[R]


  }

  def Connection(version: Version) = AsyncConnection(version)
}


trait Blocking {

  //implicit def produceBlockingSeq[T](p: ProduceSequence[T])(implicit c: BlockingConnection, mf: Manifest[T]) = new ProduceBlockingSeq[T](p)

  //implicit def produceBlocking[T](p: Produce[T])(implicit c: BlockingConnection, mf: Manifest[T]) = new ProduceBlocking[T](p)

  implicit class ProduceBlockingSeq[T](p: ProduceSequence[T])(implicit mf: Manifest[T]) {

    private def cast[R](implicit c: Connection, mf: Manifest[R]): Either[RethinkError, Seq[R]] = p.toQuery[R].toResult.asInstanceOf[Either[RethinkError, Seq[R]]]

    //def run(implicit c: Connection, mf: Manifest[T], d: DummyImplicit): Either[RethinkError, Seq[T]] = cast[T]


    def as[R <: T](implicit c: Connection, mf: Manifest[R]): Either[RethinkError, Seq[R]] = cast[R]

    def asOpt[R <: T](implicit c: Connection, tt: Manifest[R]) = as[R] fold(x => None, Some(_))


  }

  implicit class ProduceBlocking[T](p: Produce[T])(implicit mf: Manifest[T]) {

    private def cast[R](implicit c: Connection, mf: Manifest[R]): Either[RethinkError, R] = p.toQuery[R].toResult.asInstanceOf[Either[RethinkError, R]]

    //def run[R <: T](implicit c: Connection, mf: Manifest[R]) = cast[R]


    def as[R <: T](implicit c: Connection, tt: Manifest[R]) = cast[R]

    def asOpt[R <: T](implicit c: Connection, tt: Manifest[R]) = as[R] fold(x => None, Some(_))

  }

  def Connection(version: Version) = BlockingConnection(version)
}
