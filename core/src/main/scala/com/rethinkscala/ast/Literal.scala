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

  def +(other: Addition) = add(other)

  def add(other: Addition) = Add(underlying, other)

  def +=(other: Addition) = Add(underlying, other)
}


trait Literal extends Addition {
  def unary_~ = not

  override val underlying = this

  def not = Not(underlying)


  def ===(other: Literal) = eq(other)

  def eq(other: Literal) = Eq(underlying, other)

  def !=(other: Literal) = ne(other)

  def =!=(other: Literal) = ne(other)


  def ne(other: Literal) = Ne(underlying, other)

  def <(other: Literal) = lt(other)

  def lt(other: Literal) = Lt(underlying, other)

  def <=(other: Literal) = lte(other)

  def lte(other: Literal) = Le(underlying, other)

  def >(other: Literal) = gt(other)

  def gt(other: Literal) = Gt(underlying, other)

  def >=(other: Literal) = gte(other)

  def gte(other: Literal) = Ge(underlying, other)


}

trait Strings extends Literal {


  override val underlying = this
  //

  // def ===(regexp: String) = find(regexp)
  def find(regexp: Regex): Match = find(regexp.toString())

  def find(regex: String): Match = Match(underlying, regex)
}

trait Numeric extends Literal with Multiply with Binary {


  override val underlying = this

  def -(other: Numeric) = sub(other)

  def -(other: Double) = sub(other)

  def sub(other: Numeric): Sub = Sub(underlying, other)

  def sub(other: Double): Sub = Sub(underlying, other)

  def /(other: Numeric) = div(other)

  def /(other: Double) = div(other)

  def div(other: Numeric): Div = Div(underlying, other)

  def div(other: Double): Div = Div(underlying, other)

  def %(other: Numeric) = mod(other)

  def %(other: Double) = mod(other)

  def mod(other: Numeric) = Mod(underlying, other)

  def mod(other: Double) = Mod(underlying, other)
}

trait LiteralSequence[T] extends Sequence[T]