package com.rethinkscala.ast

import scala.util.matching.Regex

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 2/24/14
 * Time: 8:14 AM 
 */


trait Addition extends Typed {


  override val underlying = this


}

object WithAddition{

  implicit class ScalaWithAddition[-T,+R](underlying:WithAddition[T,R]){

    def +(other: T) = underlying.add(other)

    def +=(other: T) = underlying.add(other)
  }
}

trait WithAddition[-T, +R] extends Addition {



  def add(other: T): R
}


object Literal{

  
}


trait Literal extends Addition  {
  def unary_~ = not
  def ===(other: Literal) = eq(other)
  def !=(other: Literal) = ne(other)

  def =!=(other: Literal) = ne(other)

  def <(other: Literal) = lt(other)

  def <=(other: Literal) = lte(other)
  def >=(other: Literal) = gte(other)
  def >(other: Literal) = gt(other)

  override val underlying = this

  def not = Not(underlying)




  def eq(other: Literal) = Eq(underlying, other)




  def ne(other: Literal) = Ne(underlying, other)



  def lt(other: Literal) = Lt(underlying, other)

  def lte(other: Literal) = Le(underlying, other)



  def gt(other: Literal) = Gt(underlying, other)



  def gte(other: Literal) = Ge(underlying, other)


}


object Strings{
    implicit class Implicits(s:Strings){

    }
}


trait Strings extends Literal {


  override val underlying = this
  //


  def +(other: Strings) = add(other)
 // def +(other:String) = add(other)

  def +=(other:String) = add(other)
  //def +=(other: Strings) =add(other)

  def find(regexp: Regex): Match = find(regexp.toString())

  def add(other: Strings): StringAdd = StringAdd(underlying, other)
  def add(other:String):StringAdd = StringAdd(underlying,other)

  def split = Split(underlying)
  def split(delimiter:String) = Split(underlying,delimiter)
  def split(limit:Int) = Split(underlying,limit=limit)
  def split(delimiter:String,limit:Int) = Split(underlying,delimiter,limit)



  def find(regex: String): Match = Match(underlying, regex)
}

object Numeric{
    /*
  implicit class Implicits(n:Numeric){
    def +(other: Numeric) = n.add(other)

    def +=(other: Numeric) = n.add(other)
    def -(other: Numeric) = n.sub(other)


    def /(other: Numeric) = n.div(other)


    def %(other: Numeric) = n.mod(other)


  }   */

}


/*
object Numeric{

  implicit def toScalaNumeric(value:Numeric) = new Numeric with ScalaNumeric {
    override val underlying: Numeric = value
  }
} */

trait Numeric extends Literal with Multiply with Binary  {


  override val underlying = this



  def +(other: Numeric) = underlying.add(other)

  def +=(other: Numeric) = underlying.add(other)
  def -(other: Numeric) = underlying.sub(other)


  def /(other: Numeric) = underlying.div(other)


  def %(other: Numeric) = underlying.mod(other)

  def add(other: Numeric) = NumericAdd(underlying, other)
  def add(other: Double) = NumericAdd(underlying, other)
  def add(other: Int) = NumericAdd(underlying, other)
  def add(other: Float) = NumericAdd(underlying, other)
  def add(other: Long) = NumericAdd(underlying, other)



  def sub(other: Numeric): Sub = Sub(underlying, other)
  def sub(other: Double): Sub = Sub(underlying, other)
  def sub(other:Float):Sub = Sub(underlying,other)
  def sub(other:Int):Sub = Sub(underlying,other)
  def sub(other:Long): Sub = Sub(underlying,other)



  def div(other: Numeric): Div = Div(underlying, other)
  def div(other: Double): Div = Div(underlying, other)
  def div(other: Int): Div = Div(underlying, other)
  def div(other: Long): Div = Div(underlying, other)
  def div(other: Float): Div = Div(underlying, other)



  def mod(other: Numeric) = Mod(underlying, other)
  def mod(other: Double) = Mod(underlying, other)
  def mod(other:Int) = Mod(underlying, other)
  def mod(other: Long) = Mod(underlying, other)
  def mod(other: Float) = Mod(underlying, other)


}

trait LiteralSequence[T] extends Sequence[T]