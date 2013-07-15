package com.rethinkscala

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/13/13
 * Time: 11:00 AM
 *
 */
class Schema {



  def tableNameFromClass(c: Class[_]):String =
    c.getSimpleName
 /*
  protected def table[T]()(implicit manifestT: Manifest[T]: Table[T] =
    table(tableNameFromClass(manifestT.runtimeClass))(manifestT)

  protected def table[T](name: String)(implicit manifestT: Manifest[T]): Table[T] = {
    val typeT = manifestT.erasure.asInstanceOf[Class[T]]
    val t = new Table[T](name, typeT, this, None, ked.keyedEntityDef)
    _addTable(t)
    _addTableType(typeT, t)
    t
  }*/
}
