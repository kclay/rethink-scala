package com.rethinkdb


import com.rethinkdb.ast.Produce
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Success, Failure}


abstract class Query[R]{

  val connection:Connection

  def iterator: Iterator[R]
  val ast:ql2.Term

  def single:R ={
    val i = iterator
    val r = i.next
    //if(i.hasNext)

    r
  }

  def singleOption: Option[R] = {
    val i = iterator
    val res =
      if(i.hasNext)
        Some(i.next)
      else
        None

   // if(i.hasNext)
     // org.squeryl.internals.Utils.throwError("singleOption called on query returning more than one row : \n" + statement)
    res
  }

  def headOption = {
    val i = iterator
    if(i.hasNext)
      Some(i.next)
    else
      None
  }

  def toResult[R]:Either[RethinkError,Option[R]]

}

case class BlockingQuery[R](term:Term,connection:Connection) extends Query[R]{
  def iterator: Iterator[R] = ???

    override lazy val  ast: ql2.Term = term.ast

  def toResult[R] = toResult(Duration.Inf)
  def toResult[R](atMost:Duration):Either[RethinkError,Option[R]] =  {

    val f = connection.write[R](term)
    Await.ready(f,atMost)
    val r = f.value match{
      case Some(Failure(e:RethinkError))=>Left(e)
      case Some(Success(r:Option[R]))=>Right(r)
      case Some(Failure(e:Exception))=>Left(RethinkRuntimeError(e.getMessage,term))
      case _=>Left(RethinkRuntimeError("Opps",term))
    }
     r

  }


}



