package com.rethinkscala.ast

import com.rethinkscala.{ InsertResult, DocumentConversion, BinaryConversion, TermMessage }

import ql2.Term.TermType

trait WithDB {
  val scopedDB: Option[DB]
}

case class DB(name: String) extends TermMessage {
  override lazy val args = buildArgs(name)

  def termType = TermType.DB

  def newTable(name: String, primaryKey: Option[String] = None, dataCenter: Option[String] = None, cacheSize: Option[Int] = None) = {
    TableCreate(name, primaryKey, dataCenter, cacheSize, Some(this))
  }

  def ^^(name: String, primaryKey: Option[String], dataCenter: Option[String], cacheSize: Option[Int]) = this newTable (name, primaryKey, dataCenter, cacheSize)

  def dropTable(name: String) = TableDrop(name)

  def ^-(name: String) = this dropTable (name)

  def table(name: String, useOutDated: Boolean = false) = Table(name, Some(useOutDated), Some(this))

  def ^(name: String, useOutDated: Boolean = false) = this table (name, useOutDated)

}

// TODO FunCall

case class Table(name: String, useOutDated: Option[Boolean] = None, scopedDB: Option[DB] = None) extends TermMessage
    with ProduceSequence

    with WithDB {

  override lazy val args = buildArgs(name)
  override lazy val optargs = buildOptArgs(Map("use_outdated" -> useOutDated))

  def termType = TermType.TABLE

  def insert(records: Seq[Map[String, Any]], upsert: Boolean = false) = Insert(this, records, Some(upsert))

  def ++(records: Seq[Map[String, Any]], upsert: Boolean = false) = this insert (records, upsert)

  def <<(attribute: String) = Get(this, attribute)
}

case class DBCreate(name: String) extends TermMessage with ProduceBinary with BinaryConversion {
  override lazy val args = buildArgs(name)
  val resultField = "created"

  def termType = TermType.DB_CREATE
}

case class DBDrop(name: String) extends TermMessage with ProduceBinary with BinaryConversion {
  override lazy val args = buildArgs(name)
  val resultField = "dropped"

  def termType = TermType.DB_DROP
}

class DBList extends TermMessage with ProduceSequence {
  def termType = TermType.DB_LIST
}

case class TableCreate(name: String, primaryKey: Option[String] = None, dataCenter: Option[String] = None, cacheSize: Option[Int] = None, db: Option[DB] = None)
    extends TermMessage with ProduceBinary
    with WithDB with BinaryConversion {
  val resultField = "created"

  val scopedDB = db
  override lazy val args = buildArgs(name)
  override lazy val optargs = buildOptArgs(Map("primary_key" -> primaryKey, "datacenter" -> dataCenter, "cache_size" -> cacheSize))

  def termType = TermType.TABLE_CREATE

}

case class TableDrop(name: String) extends TermMessage with ProduceBinary with BinaryConversion {
  val resultField = "dropped"
  override lazy val args = buildArgs(name)

  def termType = TermType.TABLE_DROP

}

case class TableList(db: DB) extends TermMessage with ProduceSequence {
  //override lazy val args=buildArgs(db)
  def termType = TermType.TABLE_LIST
}

case class Insert(table: Table, records: Seq[Map[String, Any]], upsert: Option[Boolean] = None) extends TermMessage
    with ProduceDocument

    with DocumentConversion[InsertResult] {

  override lazy val args = buildArgs(table, records)
  override lazy val optargs = buildOptArgs(Map("upsert" -> upsert))

  def termType = TermType.INSERT

  implicit def optionInt2Int(i: Option[Any]) = i.get.asInstanceOf[Int]

  implicit def optionAny2OptionString(i: Option[Any]) = i.asInstanceOf[Option[String]]

  implicit def optionAny2Seq(i: Option[Any]) = i match {
    case Some(errors: Seq[Any]) => errors
    case _                      => Seq.empty[Any]
  }

  def convert(value: Map[String, Any]): InsertResult = InsertResult(
    value.get("inserted"), value.get("replaced"), value.get("unchanged"), value.get("errors"), value.get("first_error"),
    value.get("generated_keys"), value.get("deleated"), value.get("skipped")

  )

}

/*
case class SIndexCreate(table:Table,field:String) extends TermMessage with MethodTerm {
  //override lazy val args=buildArgs(db)
  def termType = p.TermMessage.TermType
}
*/ 