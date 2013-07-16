package com.rethinkscala

import com.rethinkscala.ast._
import com.rethinkscala.net.{Connection, RethinkClientError, RethinkError, Document}
import java.lang.Exception
import scala.collection.mutable.ArrayBuffer

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/13/13
 * Time: 11:00 AM
 *
 */


class Schema {


  protected implicit def thisSchema = this

  implicit def doc2Active[A <: Document](a: A)(implicit m: Manifest[A], c: Connection) =
    new ActiveRecord(a, m)

  class ActiveRecord[T <: Document](o: T, m: Manifest[T])(implicit c: Connection) {
    private def _performAction[R](action: (Table[T]) => Produce[R])(implicit m: Manifest[R]): Either[Exception, R] = (
      _tableTypes get (m.runtimeClass) map {
        table: Table[_] => action(table.asInstanceOf[Table[T]]).run

      }) getOrElse (Left(new Exception(s"No Table found in Schema for this ${m.runtimeClass}")))

    /**
     * Same as {{{table.insert(a)}}}
     */
    def save =
      _performAction(_.insert(o))

    /**
     * Same as {{{table.update(a)}}}
     */
    /* def update =
       _performAction(_.update(o))  */

    // def save: Option[T]
  }

  private val _tableTypes = new collection.mutable.HashMap[Class[_], Table[_]]

  private[rethinkscala] def _addTableType[T <: Document](typeT: Class[_], t: Table[T]) =
    _tableTypes += ((typeT, t))

  private val tables = new ArrayBuffer[Table[_]]

  def tableNameFromClass(c: Class[_]): String =
    c.getSimpleName.replaceAll("^([^A-Za-z_])", "_$1").replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2").replaceAll("([a-z0-9])([A-Z])", "$1_$2").toLowerCase

  protected def table[T <: Document](implicit manifestT: Manifest[T]): Table[T] =
    table(tableNameFromClass(manifestT.runtimeClass))(manifestT)

  protected def table[T <: Document](name: String, useOutDated: Option[Boolean] = None, db: Option[String] = None
                                      )(implicit manifestT: Manifest[T]): Table[T] = {


    val typeT = manifest.runtimeClass
    val t = new Table[T](name, useOutDated, db.map(DB(_)))
    tables.append(t)
    _addTableType[T](typeT, t)
    t
  }

  def on[T <: Document](table: Table[T])(f: TableView[T] => Unit) = ???

  def db(name: String) = r.db(name)

  def setup(implicit c: Connection) = tables.foreach {
    t => {
      t.db.map(_.create.run match {
        case Left(e) => println(e)
        case Right(v) => println(s"Db ${t.name} created -> $v")
      })
      t.create.run match {
        case Left(e) => println(e)
        case Right(v) => println(s"Table ${t.name} created -> $v")
      }
    }
  }


}

class TableView[T <: Document](table: Table[T]) {

  def db(name: String) = ???

  def index(name: String) = ???

  def index(name: String, f: Var => Typed) = ???
}
