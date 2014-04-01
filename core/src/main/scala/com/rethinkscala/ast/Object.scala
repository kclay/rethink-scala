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


  def hasFields(values: String*) = HasFields(underlying, values)

  def keys = Keys(underlying)

}

trait Hash {
  self: Typed =>
  type FieldProduce

  override val underlying = this

  def field(name: String): this.FieldProduce

  def apply(name: String)= field(name)

  def \(name: String) = field(name)
}
