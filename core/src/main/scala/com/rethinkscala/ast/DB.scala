package com.rethinkscala.ast

import com.rethinkscala.TermMessage

import ql2.Ql2.Term.TermType
import com.rethinkscala.net.BinaryConversion
import com.rethinkscala.net.Document

trait WithDB {
  val db: Option[DB]
}

case class DB(name: String) extends TermMessage {
  override lazy val args = buildArgs(name)

  def termType = TermType.DB

  def tableCreate(name: String, primaryKey: Option[String] = None,
                  durability: Option[Durability.Kind] = None, cacheSize: Option[Int] = None, dataCenter: Option[String] = None) = {
    TableCreate(name, primaryKey, durability, cacheSize, dataCenter, Some(this))
  }

  def create = DBCreate(name)

  def drop = DBDrop(name)

  def ^+(name: String) = tableCreate(name)

  def tableDrop(name: String) = TableDrop(name, Some(this))

  def ^-(name: String) = this tableDrop (name)

  def table[T <: Document](name: String, useOutDated: Boolean = false) = Table[T](name, Some(useOutDated), Some(this))

  def ^(name: String, useOutDated: Boolean = false) = this table(name, useOutDated)

}

case class DBCreate(name: String)

  extends ProduceBinary with BinaryConversion {
  override lazy val args = buildArgs(name)
  val resultField = "created"

  def termType = TermType.DB_CREATE
}

case class DBDrop(name: String) extends TermMessage with ProduceBinary with BinaryConversion {
  override lazy val args = buildArgs(name)
  val resultField = "dropped"

  def termType = TermType.DB_DROP
}

case class DBList(db: Option[DB] = None) extends ProduceSequence[String] with WithDB {

  override protected val extractArgs = false

  def termType = TermType.DB_LIST
}

/*
case class SIndexCreate(table:Table,field:String) extends TermMessage with MethodTerm {
  //override lazy val args=buildArgs(db)
  def termType = p.TermMessage.TermType
}
*/ 