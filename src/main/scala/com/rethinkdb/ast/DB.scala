package com.rethinkdb.ast

import com.rethinkdb.{TermMessage, TopLevelTerm, MethodTerm}

import ql2.Term.TermType


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 4/2/13
 * Time: 7:39 PM
 * To change this template use File | Settings | File Templates.
 */

trait WithDB {
  val scopedDB: Option[DB]
}

case class DB(name: String) extends TermMessage {
  override lazy val args = buildArgs(name)

  def toDatum=StringDatum(name)

  def termType = TermType.DB


  def newTable(name: String, primaryKey: Option[String] = None, dataCenter: Option[String] = None, cacheSize: Option[Int] = None) = {
    TableCreate(this, name, primaryKey, dataCenter, cacheSize)
  }

  def ^^(name: String, primaryKey: Option[String], dataCenter: Option[String], cacheSize: Option[Int]) = this newTable(name, primaryKey, dataCenter, cacheSize)


  def dropTable(name: String) = TableDrop(name)

  def ^-(name: String) = this dropTable (name)

  def table(name: String, useOutDated: Boolean = false) = Table(this, name, Some(useOutDated))

  def ^(name: String, useOutDated: Boolean = false) = this table(name, useOutDated)


}

// TODO FunCall


case class Table(db: DB, name: String, useOutDated: Option[Boolean] = None) extends TermMessage with MethodTerm with WithDB {
  val scopedDB = Some(db)
  override lazy val args = buildArgs(db, name)
  override lazy val optargs = buildOptArgs(Map("use_outdated" -> useOutDated))

  def termType = TermType.TABLE

  def insert(records: Seq[Map[String, Any]], upsert: Boolean = false) = Insert(this, records, Some(upsert))

  def ++(records: Seq[Map[String, Any]], upsert: Boolean = false) = this insert(records, upsert)

  def <<(attribute: String) = Get(this, attribute)
}

case class DBCreate(name: String) extends TopLevelTerm {
  override lazy val args = buildArgs(name)

  def termType = TermType.DB_CREATE
}

case class DBDrop(name: String) extends TopLevelTerm {
  override lazy val args = buildArgs(name)

  def termType = TermType.DB_DROP
}

class DBList extends TopLevelTerm {
  def termType = TermType.DB_LIST
}

case class TableCreate(db: DB, name: String, primaryKey: Option[String] = None, dataCenter: Option[String] = None, cacheSize: Option[Int] = None) extends TermMessage with WithDB {
  val scopedDB = Some(db)
  override lazy val args = buildArgs(db, name)
  override lazy val optargs = buildOptArgs(Map("primary_key" -> primaryKey, "datacenter" -> dataCenter, "cache_size" -> cacheSize))

  def termType = TermType.TABLE_CREATE
}

case class TableDrop(name: String) extends TermMessage with MethodTerm {
  override lazy val args = buildArgs(name)

  def termType = TermType.TABLE_DROP
}

case class TableList(db: DB) extends TermMessage with MethodTerm {
  //override lazy val args=buildArgs(db)
  def termType = TermType.TABLE_LIST
}

case class Insert(table: Table, records: Seq[Map[String, Any]], upsert: Option[Boolean] = None) extends TermMessage {

  override lazy val args = buildArgs(table, records)
  override lazy val optargs = buildOptArgs(Map("upsert" -> upsert))

  def termType = TermType.INSERT
}

/*
case class SIndexCreate(table:Table,field:String) extends TermMessage with MethodTerm {
  //override lazy val args=buildArgs(db)
  def termType = p.TermMessage.TermType
}
*/