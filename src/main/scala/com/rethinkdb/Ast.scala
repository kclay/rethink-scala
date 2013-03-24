package com.rethinkdb

import ql2.{Ql2 => p}



/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/19/13
 * Time: 12:06 PM 
 */

object Conversions{
  import Tokens._
  import Ast.{Term,DB}

  implicit def termType2TokenType(termType:p.Term.TermType):TermTokenType = TermTokenType(termType)
  implicit def termTokenType2TermType(token:TermTokenType):p.Term.TermType = token.termType

  implicit def datumType2TokenType(datumType:p.Datum.DatumType):DatumTokenType = DatumTokenType(datumType)
  implicit def datumTokenType2DatumType(token:DatumTokenType):p.Datum.DatumType = token.datumType

  implicit def args2Token(args:Iterable[Token]):Args = Args(args)

  implicit def optArgs2Token(optargs:Iterable[AssocPairToken]) = OptArgs(optargs)
  implicit def termBaseToTerm(base: Term): p.Term = {
      val builder = p.Term.newBuilder()
      base.compile(builder)
      builder.build()


    }

    implicit def optargToAssocPair(i: (String, Term)): p.Term.AssocPair = {

      p.Term.AssocPair.newBuilder().setKey(i._1).setVal(i._2).build()
    }
    implicit def dbToQueryAssocPair(db:DB):p.Query.AssocPair={
      p.Query.AssocPair.newBuilder().setKey("db").setVal(db).build
    }
}
object Tokens{

  sealed trait Token
  sealed trait TermBlock extends Token
  trait TokenType extends Token{
    type T
    def name:String
    def value:T
  }

  case class TermTokenType(termType:p.Term.TermType) extends TokenType {
    type T =p.Term.TermType
    def name:String =termType.name
    def value = termType
  }

  case class DatumTokenType(datumType:p.Datum.DatumType) extends TokenType {
    type T = p.Datum.DatumType
    def name:String = datumType.name
    def value = datumType
  }
  trait AssocPairToken extends Token{
    import Ast.Expr
    val key:String
    val value:Any
    lazy val token =Expr(value)


    import Ast.Expr

    type T


    trait Builder{

      val get:Any
    }


    def pair:T



  }
  case class TermAssocPairToken(key:String,value:Any) extends AssocPairToken{

    type T = p.Term.AssocPair


    private lazy val builder=new Builder{

      val get = {
        val term=p.Term.newBuilder
        token.compile(term)
        p.Term.AssocPair.newBuilder.setKey(key).setVal(term.build).build
      }
    }
    def unwrap:p.Term.AssocPair = pair.asInstanceOf[p.Term.AssocPair]
    def pair:T = builder.get.asInstanceOf[T]


  }

  case class DatumAssocPairToken(key:String,value:Any) extends AssocPairToken{
    type T = p.Datum.AssocPair

    private lazy val builder=new Builder{
      val term=p.Term.newBuilder
      token.compile(term)

      val get = p.Datum.AssocPair.newBuilder.setKey(key).setVal(p.Datum.newBuilder.build).build
    }
    def unwrap:p.Datum.AssocPair = pair.asInstanceOf[p.Datum.AssocPair]
    def pair:T = builder.get.asInstanceOf[T]

  }

  case class Args(args:Iterable[Token]) extends Token
  case class OptArgs(optargs:Iterable[AssocPairToken]) extends Token
  // implicit def tokenTypeToTermType(tokenType:Either[p.Term.TermType,p.Datum.DatumType]):p.Term.TermType =tokenType.left



}

object Ast {


  import com.rethinkdb.Tokens._
  import com.rethinkdb.Conversions._

  trait Composable {
    def compose(args: Seq[Term], optargs: Map[String, Term]) = {
      ""
    }
  }


  /*case class T(values: Any*) extends Iterator[String] {
    def hasNext = ???

    var pos = 0

    def next() = {
      for (itr <- values(0)) {
        for (sub <- itr) yield sub
        for (token <- itr)
      }
    }
  }*/

  object Expr {
    /*import reflect._
    class Def[C](implicit desired : Manifest[C]) {
       def unapply[X](c : X)(implicit m : Manifest[X]) : Option[C] = {
           def sameArgs = desired.typeArguments.zip(m.typeArguments).forall {case (desired,actual) => desired >:> actual}
           if (desired >:> m && sameArgs) Some(c.asInstanceOf[C])
           else None
         }
       }

    val DefMap=new Def[Map[String,Any]]
    val DefSeq=new Def[Seq[Any]]    */
    def apply(term: Term):Term = term

    def apply(value:Seq[Any]):Term = MakeArray(value)
    def apply(value:Map[String,Any]):Term = MakeObj(value)
    def apply(a: Any): Term = {
      val b = a
      a match{
        case t:Term => t
        case s:Seq[_]=>MakeArray(s)
        case m:Map[_,_]=>MakeObj(m.asInstanceOf[Map[String,Any]])
        case a:Any => Datum(a)


      }
    }

  }

  trait Term extends TermBlock{
    def ast:String = "Term"
    lazy val args = Seq.empty[Term]


    protected def buildArgs(args: Any*) = for (a <- args) yield Expr(a)

    lazy val optargs = Iterable.empty[AssocPairToken]


    protected def buildOptArgs(optargs: Map[String, Any]):Iterable[AssocPairToken] = optargs.filter(_._2 ==None) collect {
      case (key: String, value: Any) =>  optArgsBuilder (key,value)
    }
    def optArgsBuilder(key:String,value:Any):AssocPairToken =  TermAssocPairToken(key,value)


    def termType: TokenType


    def !(connection:Connection)={
      connection ? this
    }
    def run() = {

      None
    }



    type TermType = p.Term.TermType
    def compile(builder:p.Term.Builder)= {

     builder.setType(termType.value.asInstanceOf[TermType])
     for (a <- args) builder.addArgs(a)
      //for (o <- optargs) builder.addOptargs(o)
     // builder.build()



    }

    def ==(other: Term) = Eq(this, other)

    def !=(other: Term) = Ne(this, other)

    def <(other: Term) = Lt(this, other)

    def <=(other: Term) = Le(this, other)

    def >(other: Term) = Gt(this, other)

    def >=(other: Term) = Ge(this, other)

    def ~(other: Term) = Not(this)

    def +(other: Term) = Add(this, other)

    def >+(other: Term) = Add(other, this)

    def -(other: Term) = Sub(this, other)

    def >-(other: Term) = Sub(other, this)

    def *(other: Term) = Mul(this, other)

    def >*(other: Term) = Mul(other, this)

    def /(other: Term) = Div(this, other)

    def >/(other: Term) = Div(other, this)

    def %(other: Term) = Mod(this, other)

    def >%(other: Term) = Mod(other, this)

    def &(other: Term) = All(this, other)

    def &&(other: Term) = All(other, this)

    def &>(other: Term) = this && other

    // or
    def |(other: Term) = RAny(this, other)

    // right or
    def >|(other: Term) = RAny(other, this)

    def contains(attr: Seq[Any]) = Contains(this, attr)

  }


  sealed trait BiOpTerm extends Composable  with Token

  sealed trait TopLevelTerm extends Composable with Token

  sealed trait MethodTerm extends Composable with Token

  sealed trait ExprWrap

  sealed trait Datum extends Term with ExprWrap with Composable {

    override def compile(builder:p.Term.Builder) = {

      build(builder.getDatumBuilder)


    }
    override def optArgsBuilder(key:String,value:Any):AssocPairToken =  DatumAssocPairToken(key,value)

    def termType:TokenType = p.Term.TermType.DATUM
    def datumType:TokenType

    def build(builder: p.Datum.Builder)
  }
  object Datum{

    def apply(a:Any):Datum= a match{

      case Some(v) => Datum(v)
      case s:String => StringDatum(s)
      case i:Int=>NumberDatum(i)
      case f:Float =>NumberDatum(f)
      case l:Long =>NumberDatum(l)
      case b:Boolean => BooleanDatum(b)

    }
  }


  object NoneDatum{
    def apply()=new NoneDatum()
  }
  class NoneDatum extends Datum {

    def datumType:DatumTokenType = p.Datum.DatumType.R_NULL
    def build(builder: p.Datum.Builder) ={

    }
  }

  case class BooleanDatum(value: Boolean) extends Datum {

    def datumType:DatumTokenType = p.Datum.DatumType.R_BOOL
    def build(builder: p.Datum.Builder) ={
      builder.setType(p.Datum.DatumType.R_BOOL).setRBool(value)
    }
  }

  case class NumberDatum(value: Double) extends Datum {
    def datumType:DatumTokenType = p.Datum.DatumType.R_NUM
    def build(builder: p.Datum.Builder) = {

      builder.setType(p.Datum.DatumType.R_NUM).setRNum(value)
    }
  }

  case class StringDatum(value: String) extends Datum {
    def datumType:DatumTokenType =p.Datum.DatumType.R_STR
    def build(builder: p.Datum.Builder) = {

      builder.setType(p.Datum.DatumType.R_STR).setRStr(value)
    }
  }
  //case class ArrayDatum(value:Seq[Any]) extends



  case class MakeArray(array: Seq[Any]) extends Term with Composable {
    override lazy val args = buildArgs(array: _*)

   // def datumTermType:TokenType=p.Datum
    def termType:TokenType = p.Term.TermType.MAKE_ARRAY


    // do
    // def ?(func: PartialFunction): Term = ???
    override def compose(args: Seq[Term], optargs: Map[String, Term]) = super.compose(args, optargs)
  }

  case class MakeObj(data: Map[String, Any]) extends Term {
    override lazy val optargs = buildOptArgs(data)

    def termType:TokenType = p.Term.TermType.MAKE_OBJ
  }


  case class Var(name: String) extends Term {
    override lazy val args = buildArgs(name)

    def termType:TokenType = p.Term.TermType.VAR
  }

  case class JavaScript(code: String) extends Term {
     override lazy val args=buildArgs(code)
    def termType:TokenType = p.Term.TermType.JAVASCRIPT
  }

  case class UserError(error: String) extends Term {
    override lazy val args=buildArgs(error)
    def termType:TokenType = p.Term.TermType.ERROR
  }

  class ImplicitVar extends Term with Composable {
    def termType:TokenType = p.Term.TermType.IMPLICIT_VAR
  }


  abstract class BiOperationTerm(left: Term, right: Term) extends Term with BiOpTerm {
    override lazy val args = buildArgs(left, right)
  }

  case class Eq(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType:TokenType = p.Term.TermType.EQ
  }

  case class Ne(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType:TokenType = p.Term.TermType.NE
  }

  case class Lt(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType:TokenType = p.Term.TermType.LT
  }

  case class Le(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType:TokenType = p.Term.TermType.LE
  }

  case class Gt(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType:TokenType = p.Term.TermType.GT
  }


  case class Ge(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType:TokenType = p.Term.TermType.GE
  }

  case class Not(prev: Term) extends Term with Composable {
    override lazy val args = buildArgs(prev)

    def termType:TokenType = p.Term.TermType.NOT
  }

  case class Add(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType:TokenType = p.Term.TermType.ADD
  }

  case class Sub(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType:TokenType = p.Term.TermType.SUB
  }

  case class Mul(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType:TokenType = p.Term.TermType.MUL
  }

  case class Div(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType:TokenType = p.Term.TermType.DIV
  }

  case class Mod(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType:TokenType = p.Term.TermType.MOD
  }

  case class Append(array: Array[Any], other: Term) extends Term {
    override lazy val args = buildArgs(array, other)

    def termType:TokenType = p.Term.TermType.APPEND
  }

  case class Slice(target: Term, left: Int, right: Int) extends Term {
    override lazy val args = buildArgs(target, left, right)

    def termType:TokenType = p.Term.TermType.SLICE
  }

  case class Skip(target: Term, amount: Int) extends Term with MethodTerm {
    override lazy val args = buildArgs(target, amount)

    def termType:TokenType = p.Term.TermType.SKIP
  }


  case class Contains(target: Term, attributes: Seq[Any]) extends Term {
    override lazy val args = buildArgs(target, attributes)

    def termType:TokenType = p.Term.TermType.CONTAINS
  }


  case class All(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType:TokenType = p.Term.TermType.ALL
  }

  case class RAny(left: Term, right: Term) extends BiOperationTerm(left, right) {
    def termType:TokenType = p.Term.TermType.ANY
  }


  case class DB(name: String) extends Term {
    override lazy val args = buildArgs(name)

    def termType:TokenType = p.Term.TermType.DB

    def table_create(name: String, primaryKey: Option[String]=None, dataCenter: Option[String]=None, cacheSize: Option[Int]=None)= {
      TableCreate(name, primaryKey, dataCenter, cacheSize,Some(this))
    }

    def ^^(name: String, primaryKey: Option[String], dataCenter: Option[String], cacheSize: Option[Int]) = this table_create(name, primaryKey, dataCenter, cacheSize)


    def table_drop(name: String) = TableDrop(name)

    def ^-(name: String) = this table_drop (name)

    def table(name: String, useOutDated: Boolean = false) = Table(name, Some(useOutDated),Some(this))

    def ^(name: String, useOutDated: Boolean = false) = this table(name, useOutDated)


  }


  case class Insert(table:Table,records: Seq[Map[String, Any]], upsert: Option[Boolean] = None) extends Term {

    override lazy val args = buildArgs(table,records)
    override lazy val optargs = buildOptArgs(Map("upsert" -> upsert))

    def termType:TokenType = p.Term.TermType.INSERT
  }

  case class Get(key: String) extends Term {
    override lazy val args = buildArgs(key)

    def termType:TokenType = p.Term.TermType.GET
  }

  case class TableCreate(name: String, primaryKey: Option[String] = None, dataCenter: Option[String] = None, cacheSize: Option[Int] = None,db:Option[DB]=None) extends Term {

    override lazy val args = buildArgs(name)
    override lazy val optargs = buildOptArgs(Map("name" -> name, "primary_key" -> primaryKey, "datacenter" -> dataCenter, "cache_size" -> cacheSize))

    def termType:TokenType = p.Term.TermType.TABLE_CREATE
  }

  case class TableDrop(name: String) extends Term with MethodTerm {
    override lazy val args = buildArgs(name)

    def termType:TokenType = p.Term.TermType.TABLE_DROP
  }

  case class TableList(db: DB) extends Term with MethodTerm {
    //override lazy val args=buildArgs(db)
    def termType:TokenType = p.Term.TermType.TABLE_LIST
  }

  case class Table(name: String, useOutDated: Option[Boolean] = None,db:Option[DB]=None) extends Term with MethodTerm {
    override lazy val args = buildArgs(name)
    override lazy val optargs = buildOptArgs(Map("use_outdated" -> useOutDated))

    def termType:TokenType = p.Term.TermType.TABLE

    def insert(records: Seq[Map[String, Any]], upsert: Boolean = false) = Insert(this,records, Some(upsert))

    def ++(records: Seq[Map[String, Any]], upsert: Boolean = false) = this insert(records, upsert)

    def <<(key: String) = Get(key)
  }


}
















