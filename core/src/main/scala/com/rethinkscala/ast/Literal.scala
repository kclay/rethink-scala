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

object WithAddition {

  implicit class ScalaWithAddition[-T, +R](underlying: WithAddition[T, R]) {

    def +(other: T):R = underlying.add(other)

    def +=(other: T):R = underlying.add(other)
  }

}

trait WithAddition[-T, +R] extends Addition {

  def add(other: T): R
}


trait Literal extends Addition

trait Strings extends Literal {

  override val underlying = this

  def +(other: Strings):ProduceString = add(other)

  def +=(other: String):ProduceString = add(other)

  def find(regexp: Regex): Match = find(regexp.toString())

  def add(other: Strings):ProduceString= Add(underlying, other)

  def add(other: String):ProduceString = Add(underlying, other)

  def split:Split = Split(underlying)

  def split(delimiter: String):Split = Split(underlying, delimiter)

  def split(limit: Int):Split = Split(underlying, limit = limit)

  def split(delimiter: String, limit: Int):Split = Split(underlying, delimiter, limit)

  def find(regex: String): Match = Match(underlying, regex)
}


trait Numeric extends Literal with Multiply with Binary {

  override val underlying = this

  def +(other: Numeric):ProduceNumeric = add(other)

  def +=(other: Numeric):ProduceNumeric = add(other)

  def -(other: Numeric):ProduceNumeric = sub(other)

  def /(other: Numeric):ProduceNumeric = div(other)

  def %(other: Numeric):ProduceNumeric = mod(other)

  def add(other: Numeric):ProduceNumeric = Add(underlying, other)

  def add(other: Double):ProduceNumeric = Add(underlying, other)

  def add(other: Int):ProduceNumeric = Add(underlying, other)

  def add(other: Float):ProduceNumeric = Add(underlying, other)

  def add(other: Long):ProduceNumeric = Add(underlying, other)

  def sub(other: Numeric): Sub = Sub(underlying, other)

  def sub(other: Double): Sub = Sub(underlying, other)

  def sub(other: Float): Sub = Sub(underlying, other)

  def sub(other: Int): Sub = Sub(underlying, other)

  def sub(other: Long): Sub = Sub(underlying, other)

  def div(other: Numeric): Div = Div(underlying, other)

  def div(other: Double): Div = Div(underlying, other)

  def div(other: Int): Div = Div(underlying, other)

  def div(other: Long): Div = Div(underlying, other)

  def div(other: Float): Div = Div(underlying, other)

  def mod(other: Numeric):Mod = Mod(underlying, other)

  def mod(other: Double):Mod = Mod(underlying, other)

  def mod(other: Int):Mod = Mod(underlying, other)

  def mod(other: Long):Mod = Mod(underlying, other)

  def mod(other: Float) = Mod(underlying, other)

}
