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




trait Literal extends Addition


trait Strings extends Literal {


  override val underlying = this
  //


  def +(other: Strings) = add(other)
 // def +(other:String) = add(other)

  def +=(other:String) = add(other)
  //def +=(other: Strings) =add(other)

  def find(regexp: Regex): Match = find(regexp.toString())

  def add(other: Strings)= Add(underlying, other)
  def add(other:String) = Add(underlying,other)

  def split = Split(underlying)
  def split(delimiter:String) = Split(underlying,delimiter)
  def split(limit:Int) = Split(underlying,limit=limit)
  def split(delimiter:String,limit:Int) = Split(underlying,delimiter,limit)



  def find(regex: String): Match = Match(underlying, regex)
}


trait Numeric extends Literal with Multiply with Binary  {


  override val underlying = this



  def +(other: Numeric) = add(other)

  def +=(other: Numeric) = add(other)
  def -(other: Numeric) = sub(other)


  def /(other: Numeric) = div(other)


  def %(other: Numeric) = mod(other)

  def add(other: Numeric) = Add(underlying, other)
  def add(other: Double) = Add(underlying, other)
  def add(other: Int) = Add(underlying, other)
  def add(other: Float) = Add(underlying, other)
  def add(other: Long) = Add(underlying, other)



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