package com.rethinkscala.ast


import com.rethinkscala.net.{BinaryConversion, DefaultCursor}
import com.rethinkscala.{IndexStatusResult, InsertOptions, TableOptions, _}
import ql2.Ql2.Term.TermType

// TODO FuncCall


case class Table[T <: AnyRef](name: String, useOutDated: Option[Boolean] = None,
                              db: Option[DB] = None)

  extends ProduceStreamSelection[T, DefaultCursor]
  with WithDB with TableTyped {

  override lazy val args = buildArgs(db.map(Seq(_, name)).getOrElse(Seq(name)): _*)
  override lazy val optargs = buildOptArgs(Map("use_outdated" -> useOutDated))

  def termType: TermType = TermType.TABLE

  def drop = TableDrop(name, db)

  def to[R <: AnyRef] = this.asInstanceOf[Table[R]]

  def create: TableCreate = create(TableOptions())

  def create(options: TableOptions): TableCreate = TableCreate(name, options, db)

  def insertMap(records: Seq[Map[String, Any]]) = Insert[T, T](this, Left(records), InsertOptions())

  def insertMap(records: Seq[Map[String, Any]], options: InsertOptions) = Insert[T, T](this, Left(records), options)

  def insertMap(record: Map[String, Any]) = Insert[T, T](this, Left(Seq(record)), InsertOptions())

  def insert[R <: T](records: Seq[R], options: InsertOptions) = Insert[T, R](this, Right(records), options)

  def insert[R <: T](records: Seq[R]) = Insert(this, Right(records), InsertOptions())

  def insert[R <: T](record: R) = Insert(this, Right(Seq(record)), InsertOptions())

  def insert[R <: T](record: R, options: InsertOptions): Insert[T, R] = insert[R](Seq(record), options)

  def ++=(record: T) = insert(record)

  def ++=(records: Seq[Map[String, Any]]) = this insertMap(records, InsertOptions())

  def ++=(records: Seq[T])(implicit d: DummyImplicit) = this insert records

  def \\(attribute: Typed) = get(attribute)


  def get(attribute: Typed) = Get[T](this, attribute)


  def getAll(index: String, attr: Any*): GetAll[T] = GetAll[T](this, attr, index)

  def getAll(attr: Any*): GetAll[T] = GetAll[T](this, attr, None)

  def indexes = IndexList(this)

  def indexCreate(name: String, indexFunction:Option[Typed]=None, geo: Option[Boolean]=None, multi: Option[Boolean] = None) = {
    IndexCreate(this, name, indexFunction.map(_.wrap), geo = geo, multi = multi)
  }


  def indexDrop(name: String) = IndexDrop(this, name)

  def indexStatus(indexes: String*) = IndexStatus(this, indexes)

  def indexWait(indexes: String*) = IndexWait(this, indexes)

  def indexRename(oldName: String, newName: String, overwrite: Option[Boolean] = None) = IndexRename(this, oldName, newName, overwrite)

  def sync = Sync(this)

  def sync(durability: Durability.Kind) = Sync(this, Some(durability))

}

case class TableCreate(name: String, options: TableOptions, db: Option[DB] = None)
  extends ProduceBinary
  with BinaryConversion {
  val resultField = "tables_created"

  override lazy val args = buildArgs(db.map(Seq(_, name)).getOrElse(Seq(name)): _*)

  override lazy val optargs = buildOptArgs(options.toMap)

  def withChanges = TableCreateWithChanges(this)

  def termType: TermType = TermType.TABLE_CREATE

}

case class TableDrop(name: String, db: Option[DB] = None) extends ProduceBinary with BinaryConversion {
  val resultField = "tables_dropped"
  override lazy val args = buildArgs(db.map(Seq(_, name)).getOrElse(Seq(name)): _*)

  def termType: TermType = TermType.TABLE_DROP

  def withChanges = TableDropWithChanges(this)

}

case class TableCreateWithChanges(override val term: TableCreate) extends ForwardTyped(term) with ProduceDocument[TableCreateResults]

case class TableDropWithChanges(override val term: TableDrop) extends ForwardTyped(term) with ProduceDocument[TableDropResults]

case class TableList(db: Option[DB] = None) extends ProduceDefaultSequence[String] {

  override protected val extractArgs: Boolean = false
  override lazy val args = buildArgs(db.map(Seq(_)).getOrElse(Seq()): _*)

  def termType: TermType = TermType.TABLE_LIST
}

/** Create a new secondary index on this table.
  * @param target
  * @param name
  * @param predicate
  */
case class IndexCreate(target: TableTyped, name: String, predicate: Option[FuncWrap] = None,
                       multi: Option[Boolean] = None,
                       geo: Option[Boolean] = None)
  extends ProduceBinary with BinaryConversion {

  override lazy val args = buildArgs(predicate.map {
    f => Seq(target, name, f())
  }.getOrElse(Seq(target, name)): _*)


  override lazy val optargs = buildOptArgs(Map("multi" -> multi, "geo" -> geo))

  def termType: TermType = TermType.INDEX_CREATE

  def withGeo = copy(geo = Some(true))

  def withMulti = copy(multi = Some(true))

  val resultField = "created"
}

/** Delete a previously created secondary index of this table.
  * @param target
  * @param name
  */
case class IndexDrop(target: TableTyped, name: String) extends ProduceBinary with BinaryConversion {

  val resultField = "dropped"

  def termType: TermType = TermType.INDEX_DROP
}

/** List all the secondary indexes of this table.
  * @param target
  */
case class IndexList(target: TableTyped) extends ProduceDefaultSequence[String] {
  def termType: TermType = TermType.INDEX_LIST
}

case class IndexStatus(target: TableTyped, indexes: Seq[String]) extends ProduceDefaultSequence[IndexStatusResult] {

  override lazy val args = buildArgs((if (indexes.isEmpty) Seq(target) else indexes.+:(target)): _*)

  def termType: TermType = TermType.INDEX_STATUS
}

case class IndexWait(target: TableTyped, indexes: Seq[String]) extends ProduceDefaultSequence[IndexStatusResult] {
  override lazy val args = buildArgs((if (indexes.isEmpty) Seq(target) else indexes.+:(target)): _*)

  def termType: TermType = TermType.INDEX_WAIT
}

case class IndexRename(target: TableTyped, oldName: String, newName: String, overwrite: Option[Boolean] = None)
  extends ProduceBinary with BinaryConversion {
  val resultField = "renamed"
  override lazy val args = buildArgs(target,oldName,newName)
  override lazy val optargs = buildOptArgs(Map("overwrite" -> overwrite))

  override def termType: TermType = TermType.INDEX_RENAME
}

case class Sync(target: TableTyped, durability: Option[Durability.Kind] = None) extends ProduceBinary with BinaryConversion {

  override lazy val args = buildArgs(target)
  override lazy val optargs = buildOptArgs(Map("durability" -> durability))
  val resultField = "synced"

  def termType: TermType = TermType.SYNC
}

