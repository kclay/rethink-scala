package com.rethinkscala.ast


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 2/24/14
 * Time: 8:22 AM 
 */

trait TableTyped extends Typed


trait MapTyped extends Typed

trait Record extends Typed with Hash {

  override val underlying = this

  def pluck(attrs: String*) = Pluck(underlying, attrs)

  def pluck(m: Map[String, Any]) = Pluck(underlying, m)

  def without(attrs: String*) = Without(underlying, attrs)


  def merge(other: Record) = Merge(underlying, other)

  def merge(other: Map[String, Any]) = Merge(underlying, other)

  def +(other: Record) = merge(other)

  def hasFields(values: String*) = HasFields(underlying, values)

  def keys = Keys(underlying)

}

trait Hash {
  self: Typed =>
  type FieldProduce

  override val underlying = this

  def field(name: String): this.FieldProduce

  def apply[T <: Typed](name: String): T = GetField(underlying, name).asInstanceOf[T]

  def \(name: String) = field(name)
}
