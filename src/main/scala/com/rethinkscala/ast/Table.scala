package com.rethinkscala.ast

import com.rethinkscala.{ BinaryConversion, TermMessage }
import ql2.Term.TermType
import com.rethinkscala.Document

// TODO FuncCall

object Durability extends Enumeration {
  type Kind = Value
  val Hard = Value("hard")
  val Soft = Value("soft")
}
case class Table(name: String, useOutDated: Option[Boolean] = None,
                 db: Option[DB] = None)

    extends ProduceStreamSelection
    with WithDB {

  override lazy val args = buildArgs(name)
  override lazy val optargs = buildOptArgs(Map("use_outdated" -> useOutDated))

  def termType = TermType.TABLE

  def drop = TableDrop(name, db)

  def create: TableCreate = create()

  def create(primaryKey: Option[String] = None,
             durability: Option[Durability.Kind] = None, cacheSize: Option[Int] = None, dataCenter: Option[String] = None): TableCreate = {

    TableCreate(name, primaryKey, durability, cacheSize, dataCenter, db)

  }

  def insert(records: Seq[Map[String, Any]], upsert: Boolean = false, durability: Option[String] = None): Insert = Insert(this, Left(records), Some(upsert), durability)

  def insert(record: Document, upsert: Boolean, durability: Option[String]): Insert = insert(Seq(record), upsert, durability)

  def insert(records: Seq[Document], upsert: Boolean,
             durability: Option[String])(implicit d: DummyImplicit): Insert = Insert(this, Right(records), Some(upsert), durability)

  def insert(records: Seq[Document]) = Insert(this, Right(records), None, None)

  def insert(record: Document) = Insert(this, Right(Seq(record)), None, None)

  def ++=(record: Document): Insert = insert(record)

  def ++=(records: Seq[Map[String, Any]]): Insert = this insert (records)

  def ++=(records: Seq[Document])(implicit d: DummyImplicit): Insert = this insert (records)

  def \\(attribute: String) = get(attribute)

  def get(attribute: String) = Get(this, attribute)

  def getAll(attr: String, index: Option[String] = None) = GetAll(this, attr, index)

  def indexes = IndexList(this)

  def indexCreate(name: String, predicate: Option[Predicate] = None) = IndexCreate(this, name, predicate)

  def indexDrop(name: String) = IndexDrop(this, name)

}

case class TableCreate(name: String, primaryKey: Option[String] = None,
                       durability: Option[Durability.Kind] = None, cacheSize: Option[Int] = None, dataCenter: Option[String] = None, db: Option[DB] = None)
    extends ProduceBinary
    with WithDB with BinaryConversion {
  val resultField = "created"

  override lazy val args = buildArgs(name)
  override lazy val optargs = buildOptArgs(Map("primary_key" -> primaryKey, "datacenter" -> dataCenter, "cache_size" -> cacheSize, "durability" -> durability))

  def termType = TermType.TABLE_CREATE

}

case class TableDrop(name: String, db: Option[DB] = None) extends ProduceBinary with BinaryConversion with WithDB {
  val resultField = "dropped"
  override lazy val args = buildArgs(name)

  def termType = TermType.TABLE_DROP

}

case class TableList(db: Option[DB] = None) extends TermMessage with ProduceAnySequence with WithDB {

  override protected val extractArgs: Boolean = false

  def termType = TermType.TABLE_LIST
}

/** Create a new secondary index on this table.
 *  @param target
 *  @param name
 *  @param predicate
 */
case class IndexCreate(target: Table, name: String, predicate: Option[Predicate] = None) extends ProduceBinary with BinaryConversion {

  override lazy val args = buildArgs(predicate.map {
    f => Seq(target, name, f())
  }.getOrElse(Seq(target, name)): _*)

  def termType = TermType.INDEX_CREATE

  val resultField = "created"
}

/** Delete a previously created secondary index of this table.
 *  @param target
 *  @param name
 */
case class IndexDrop(target: Table, name: String) extends ProduceBinary with BinaryConversion {

  val resultField = "dropped"

  def termType = TermType.INDEX_DROP
}

/** List all the secondary indexes of this table.
 *  @param target
 */
case class IndexList(target: Table) extends ProduceDocument {
  def termType = TermType.INDEX_LIST
}