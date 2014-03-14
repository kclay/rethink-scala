package com.rethinkscala.ast

import ql2.Ql2.Term.TermType
import com.rethinkscala.net.BinaryConversion
import com.rethinkscala._
import com.rethinkscala.InsertOptions
import com.rethinkscala.TableOptions
import com.rethinkscala.IndexStatusResult

// TODO FuncCall


case class Table[T <: Document](name: String, useOutDated: Option[Boolean] = None,
                                db: Option[DB] = None)

  extends ProduceStreamSelection[T]
  with WithDB with TableTyped {

  override lazy val args = buildArgs(db.map(Seq(_, name)).getOrElse(Seq(name)): _*)
  override lazy val optargs = buildOptArgs(Map("use_outdated" -> useOutDated))

  def termType = TermType.TABLE

  def drop = TableDrop(name, db)

  def create: TableCreate = create(TableOptions())

  def create(options: TableOptions): TableCreate = TableCreate(name, options, db)

  def insertMap(records: Seq[Map[String, Any]]) = Insert[T,T](this, Left(records), InsertOptions())
  def insertMap(records: Seq[Map[String, Any]], options: InsertOptions) = Insert[T,T](this, Left(records), options)



  def insert[R<: T](records: Seq[R], options: InsertOptions) = Insert[T,R](this, Right(records), options)

  def insert[R <: T](records: Seq[R]) = Insert(this, Right(records), InsertOptions())

  def insert[R<:T](record: R) = Insert(this, Right(Seq(record)), InsertOptions())
  def insert[R<:T](record: R,  options: InsertOptions): Insert[T,R] = insert[R](Seq(record), options)

  def ++=(record: T) = insert(record)

  def ++=(records: Seq[Map[String, Any]]) = this insertMap(records, InsertOptions())

  def ++=(records: Seq[T])(implicit d: DummyImplicit) = this insert records

  def \\(attribute: Any) = get(attribute)


  def get(attribute: Any) = Get[T](this, attribute)


  def getAll(index: String,attr: Any*): GetAll[T] = GetAll[T](this, attr, index)

  def getAll(attr: Any*): GetAll[T] = GetAll[T](this, attr, None)

  def indexes = IndexList(this)


  def indexCreate(name: String, predicate: Predicate) = IndexCreate(this, name, Option(predicate), None)

  def indexCreate(name: String, multi: Boolean, predicate: Predicate) = IndexCreate(this, name, Option(predicate), Some(multi))

  def indexCreate(name: String, multi: Boolean) = IndexCreate(this, name, None, Some(multi))


  def indexCreate(name: String) = IndexCreate(this, name)

  def indexDrop(name: String) = IndexDrop(this, name)

  def indexStatus = IndexStatus(this, Seq())

  def indexStatus(indexes: String*) = IndexStatus(this, indexes)

 // def indexWait = IndexWait(this, Seq())

  def indexWait(indexes: String*) = IndexWait(this, indexes)

  def sync = Sync(this)

}

case class TableCreate(name: String, options: TableOptions, db: Option[DB] = None)
  extends ProduceBinary
  with BinaryConversion {
  val resultField = "created"

  override lazy val args = buildArgs(db.map(Seq(_, name)).getOrElse(Seq(name)): _*)

  override lazy val optargs = buildOptArgs(options.toMap)

  def termType = TermType.TABLE_CREATE

}

case class TableDrop(name: String, db: Option[DB] = None) extends ProduceBinary with BinaryConversion {
  val resultField = "dropped"
  override lazy val args = buildArgs(db.map(Seq(_, name)).getOrElse(Seq(name)): _*)

  def termType = TermType.TABLE_DROP

}

case class TableList(db: Option[DB] = None) extends ProduceSequence[String] {

  override protected val extractArgs: Boolean = false
  override lazy val args = buildArgs(db.map(Seq(_)).getOrElse(Seq()): _*)

  def termType = TermType.TABLE_LIST
}

/** Create a new secondary index on this table.
  * @param target
  * @param name
  * @param predicate
  */
case class IndexCreate(target: TableTyped, name: String, predicate: Option[Predicate] = None, multi: Option[Boolean] = None) extends ProduceBinary with BinaryConversion {

  override lazy val args = buildArgs(predicate.map {
    f => Seq(target, name, f())
  }.getOrElse(Seq(target, name)): _*)


  override lazy val optargs = buildOptArgs(Map("multi" -> multi))

  def termType = TermType.INDEX_CREATE

  val resultField = "created"
}

/** Delete a previously created secondary index of this table.
  * @param target
  * @param name
  */
case class IndexDrop(target: TableTyped, name: String) extends ProduceBinary with BinaryConversion {

  val resultField = "dropped"

  def termType = TermType.INDEX_DROP
}

/** List all the secondary indexes of this table.
  * @param target
  */
case class IndexList(target: TableTyped) extends ProduceSequence[String] {
  def termType = TermType.INDEX_LIST
}

case class IndexStatus(target: TableTyped, indexes: Seq[String]) extends ProduceSequence[IndexStatusResult] {

  override lazy val args = buildArgs((if(indexes.isEmpty) Seq(target) else Seq(target,indexes)):_*)

  def termType = TermType.INDEX_STATUS
}

case class IndexWait(target: TableTyped, indexes: Seq[String]) extends ProduceSequence[IndexStatusResult] {
  def termType = TermType.INDEX_WAIT
}

case class Sync(target: TableTyped, durability: Option[Durability.Kind] = None) extends ProduceBinary with BinaryConversion {

  override lazy val args = buildArgs(target)
  override lazy val optargs = buildOptArgs(Map("durability" -> durability))
  val resultField = "synced"

  def termType = TermType.SYNC
}