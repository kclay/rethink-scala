package com.rethinkscala.ast

import com.rethinkscala.Document


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

  def mapTo[T <: Document] = new MapToDocument[T](underlying)

  def merge(other: Any): ProduceAnyDocument = Merge.record(underlying, other)

  def hasFields(values: String*) = HasFields(underlying, values)

  def keys = Keys(underlying)


}

trait Hash {
  self: Typed =>
  type FieldProduce

  override val underlying = this

  def field(name: String): FieldProduce

  def apply(name: String): FieldProduce = field(name)

  def \(name: String) = field(name)
}
