package com.rethinkscala.ast

import com.rethinkscala._
import com.rethinkscala.net.{RethinkCursor, BinaryConversion, DefaultCursor}
import ql2.Ql2.Term.TermType

trait WithDB {
  val db: Option[DB]
}

case class DB(name: String) extends Term {
  override lazy val args = buildArgs(name)

  def termType: TermType = TermType.DB

  def tableCreate(name: String): TableCreate = tableCreate(name, TableOptions())

  def tableCreate(name: String, options: TableOptions): TableCreate = TableCreate(name, options, Some(this))

  def tables: TableList = TableList(Some(this))

  def create: DBCreate = DBCreate(name)

  def drop: DBDrop = DBDrop(name)

  def ^+(name: String): TableCreate = tableCreate(name)

  def tableDrop(name: String): TableDrop = TableDrop(name, Some(this))

  def ^-(name: String): TableDrop = this tableDrop (name)

  def table[T <: Document](name: String, useOutDated: Option[Boolean] = None): Table[T] = Table[T](name, useOutDated, Some(this))

  def ^(name: String, useOutDated: Boolean = false) = table[Document](name, Some(useOutDated))

}

case class DBCreate(name: String) extends ProduceBinary with BinaryConversion {
  override lazy val args = buildArgs(name)
  val resultField = "dbs_created"

  def withChanges: DBCreateWithChanges = DBCreateWithChanges(this)

  def termType: TermType = TermType.DB_CREATE
}

case class DBDrop(name: String) extends ProduceBinary with BinaryConversion {
  override lazy val args = buildArgs(name)
  val resultField = "dbs_dropped"

  def withChanges: DBDropWithChanges = DBDropWithChanges(this)

  def termType: TermType = TermType.DB_DROP
}


case class DBCreateWithChanges(override val term: DBCreate) extends ForwardTyped(term) with ProduceDocument[DBCreateResults]

case class DBDropWithChanges(override val term: DBDrop) extends ForwardTyped(term) with ProduceDocument[DBDropResults]

case class DBList(db: Option[DB] = None) extends ProduceSeq[String, RethinkCursor] with WithDB {

  override protected val extractArgs = false

  def termType: TermType = TermType.DB_LIST
}
