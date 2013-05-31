package com.rethinkdb.ast

import com.rethinkdb.Term

sealed trait Produce extends Term

trait ProduceSequence extends Produce with WithTransformation

trait ProduceLiteral extends Produce with WithLiteral

trait ProduceCondition extends Produce

trait ProduceAny extends ProduceSequence with ProduceCondition with ProduceLiteral with ProduceJson

trait ProduceJson extends Produce


trait WithJson {
  self: Term =>


  def attr(name: String) = GetAttr(this, name)

  def \(name: String) = attr(name)
  def append(value:Any)=Append(this,value)
  def :+(value:Any)=append(value)
}

trait WithCondition {
  self: Term =>
  def ==(other: Term) = Eq(this, other)

  def !=(other: Term) = Ne(this, other)

  def <(other: Term) = Lt(this, other)

  def <=(other: Term) = Le(this, other)

  def >(other: Term) = Gt(this, other)

  def >=(other: Term) = Ge(this, other)

  def ~(other: Term) = Not(this)

  def contains(attribute: String*) = Contains(this, attribute)

}

trait WithLiteral {
  self: Term =>


  def +(other: Term) = Add(this, other)

  def +=(other: StringDatum) = Add(this, other)

  def >+(other: Term) = Add(other, this)

  def -(other: Term) = Sub(this, other)

  def >-(other: Term) = Sub(other, this)

  def *(other: Term) = Mul(this, other)

  def >*(other: Term) = Mul(other, this)

  def /(other: Term) = Div(this, other)

  def >/(other: Term) = Div(other, this)

  def %(other: Term) = Mod(this, other)

  def >%(other: Term) = Mod(other, this)

  def &(other: Term) = All(this, other)

  def &&(other: Term) = All(other, this)

  def &>(other: Term) = this && other

  // or
  def |(other: Term) = RAny(this, other)

  // right or
  def >|(other: Term) = RAny(other, this)


  // def row(name: String) = Core.row \ name

}