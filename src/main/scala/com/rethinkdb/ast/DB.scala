package com.rethinkdb.ast

import com.rethinkdb.{TopLevelTerm, MethodTerm, Term}
import ql2.{Ql2=>p}
import com.rethinkdb.conversions.Tokens._


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

case class DB(name: String) extends Term {
  override lazy val args = buildArgs(name)

  def termType: TokenType = p.Term.TermType.DB




  def newTable(name: String, primaryKey: Option[String] = None, dataCenter: Option[String] = None, cacheSize: Option[Int] = None) = {
    TableCreate(this, name, primaryKey, dataCenter, cacheSize)
  }

  def ^^(name: String, primaryKey: Option[String], dataCenter: Option[String], cacheSize: Option[Int]) = this newTable(name, primaryKey, dataCenter, cacheSize)


  def dropTable(name: String) = TableDrop(name)

  def ^-(name: String) = this dropTable (name)

  def table(name: String, useOutDated: Boolean = false) = Table(this,name, Some(useOutDated))

  def ^(name: String, useOutDated: Boolean = false) = this table(name, useOutDated)


}

// TODO FunCall


case class Table(db:DB,name: String, useOutDated: Option[Boolean] = None) extends Term with MethodTerm with WithDB {
  val scopedDB = Some(db)
  override lazy val args = buildArgs(db,name)
  override lazy val optargs = buildOptArgs(Map("use_outdated" -> useOutDated))

  def termType: TokenType = p.Term.TermType.TABLE

  def insert(records: Seq[Map[String, Any]], upsert: Boolean = false) = Insert(this, records, Some(upsert))

  def ++(records: Seq[Map[String, Any]], upsert: Boolean = false) = this insert(records, upsert)

  def <<(attribute: String) = Get(this, attribute)
}

case class DBCreate(name: String) extends TopLevelTerm {
  override lazy val args = buildArgs(name)

  def termType: TokenType = p.Term.TermType.DB_CREATE
}

case class DBDrop(name: String) extends TopLevelTerm {
  override lazy val args = buildArgs(name)

  def termType: TokenType = p.Term.TermType.DB_DROP
}

class DBList extends TopLevelTerm {
  def termType: TokenType = p.Term.TermType.DB_LIST
}

case class TableCreate(db: DB, name: String, primaryKey: Option[String] = None, dataCenter: Option[String] = None, cacheSize: Option[Int] = None) extends Term {
  val scopedDB = Some(db)
  override lazy val args = buildArgs(db, name)
  override lazy val optargs = buildOptArgs(Map( "primary_key" -> primaryKey, "datacenter" -> dataCenter, "cache_size" -> cacheSize))

  def termType: TokenType = p.Term.TermType.TABLE_CREATE
}

case class TableDrop(name: String) extends Term with MethodTerm {
  override lazy val args = buildArgs(name)

  def termType: TokenType = p.Term.TermType.TABLE_DROP
}

case class TableList(db: DB) extends Term with MethodTerm {
  //override lazy val args=buildArgs(db)
  def termType: TokenType = p.Term.TermType.TABLE_LIST
}

case class Insert(table: Table, records: Seq[Map[String, Any]], upsert: Option[Boolean] = None) extends Term {

  override lazy val args = buildArgs(table, records)
  override lazy val optargs = buildOptArgs(Map("upsert" -> upsert))

  def termType: TokenType = p.Term.TermType.INSERT
}

/*
case class SIndexCreate(table:Table,field:String) extends Term with MethodTerm {
  //override lazy val args=buildArgs(db)
  def termType: TokenType = p.Term.TermType
}
*/