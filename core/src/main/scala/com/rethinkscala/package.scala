package com

import com.rethinkscala.ast.{ProduceSequence, GetField, Produce}
import com.rethinkscala.net.{RethinkError, Blocking, Connection}


/** Created by IntelliJ IDEA.
  * User: Keyston
  * Date: 3/19/13
  * Time: 7:32 PM
  */
package object rethinkscala {


  implicit class ProduceBlockingSeq[T](p: ProduceSequence[T])(implicit x: Connection {type QueryMode = Blocking}, mf: Manifest[T]) {

    private def cast[R](implicit c: Connection, mf: Manifest[R]): Either[RethinkError, Seq[R]] = p.toQuery[R].toResult.asInstanceOf[Either[RethinkError, Seq[R]]]

    def run(implicit c: Connection, mf: Manifest[T], d: DummyImplicit): Either[RethinkError, Seq[T]] = cast[T]


    def toSeq[R <: T](implicit c: Connection, mf: Manifest[R]): Either[RethinkError, Seq[R]] = cast[R]
  }

  // implicit def produceBlockingToSeq[T <: ProduceSequence[T]](p: ProduceSequence[T]) = new ProduceBlockingSeq[T](p)


  implicit class ProduceBlocking[T](p: Produce[T])(implicit x: Connection {type QueryMode = Blocking}, mf: Manifest[T]) {

    def asOpt[R <: T](implicit c: Connection, tt: Manifest[R]) = p.as[R].asInstanceOf[Either[Exception, R]] fold(x => None, Some(_))


    def asOpt(implicit c: Connection, mf: Manifest[T], d: DummyImplicit) = p.run.asInstanceOf[Either[Exception, T]] fold(x => None, Some(_))
  }


}
