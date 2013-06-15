package com.rethinkdb


import com.rethinkdb.ast.Produce
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration


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

  def toResult[R]:Option[R]

}

case class BlockingQuery[R](term:Term,connection:Connection) extends Query[R]{
  def iterator: Iterator[R] = ???

    override lazy val  ast: ql2.Term = term.ast

  def toResult[R] = toResult(Duration.Inf)
  def toResult[R](atMost:Duration):Option[R] =  {

    val result = Await.result(connection.write[R](term),atMost)

    val r = result match {
      case Some(a)=> Some(a.asInstanceOf[R])
      case _=>None
    }
    r
  }


}



