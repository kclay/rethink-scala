package com.rethinkdb.utils


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/20/13
 * Time: 8:29 PM 
 */

import com.rethinkdb.Ast._
import ql2.{Ql2 => p}
import com.rethinkdb.Tokens._


trait Writer {
  def <<(value: String): Int = write(value)

  def write(value: String): Int
}

class ConsoleWriter extends Writer{
  def write(value: String): Int = {
    System.out.print(value)
    value.replace("\n","").length
  }
}
class StringBufferTermWriter extends Writer {
  private val builder = StringBuilder.newBuilder

  def write(value: String):Int= {
    builder ++= value
    // get length without line ends
    value.replace("\n","").length


  }
  override def toString =builder.toString
}

case class Tree(term: Term) {

  val line = "\n"
  val open="Term {"+line
  val close = "}"

  /*sealed abstract class Leaf

  case object Line extends Leaf

  case class Text(s: String)

  case class Term(value: Ast.Term) extends Leaf

  case class TermType(value: p.Term.TermType) extends Leaf

  case class Datum(value: Ast.Datum) extends Leaf

  case class DatumType(value: p.Datum.DatumType) extends Leaf

  case class Args(args: Seq[Ast.Term]) extends Leaf
  */

  case class Indent(amount: Int) {
    def +(add: Int) = copy(amount + add)

    def ++() = this + 1
    def +++()=this + 2

    def +>(extra:Int)=copy(amount + 2 + extra)


    def -(remove: Int) = copy(amount - remove)

    def --() = this - 1
  }


  private val defaultIndent = Indent(0)

  implicit def indent2String(indent: Indent) = "#" * indent.amount
  /*


  implicit def args2Args(args: Seq[Ast.Term]) = Args(args)

  implicit def termType2TermType(termType: p.Term.TermType) = TermType(termType)

  implicit def term2Term(term: Ast.Term) = Term(term)

  implicit def datum2Datum(value:Ast.Datum) = Datum(value)

  implicit def datumType2DatumType(value: p.Datum.DatumType) = DatumType(value)
  */


  def >>(writer: Writer) {
    write(term)(writer)
  }



  private def write(token: Token, indent: Indent = defaultIndent, applyIndent: Boolean = true)(implicit writer: Writer) {
    if(applyIndent) writer << indent
    val isTermBlock = token.isInstanceOf[TermBlock]
    val offset = if(isTermBlock) writer<<"Term {"+line  else 0
    token match{

      case ap:AssocPairToken=>{
        val extra= writer << "[\""+ap.key+"\"," +line
        write(ap.token,indent+offset+extra)
        writer<<"]"


      }
      case a:MakeArray=>{
        write(a.termType,indent)
        write(a.args,indent)
      }
      case o:OptArgs=>{
        var offset = writer <<indent+"optargs = ["
        for((p,index)<-o.optargs.zipWithIndex) write(p, indent +++, index != 0)



        writer<<"];"+line
      }
      case a:Args=>{
        val offset = writer << "args = ["
        for ((term, index) <- a.args.zipWithIndex) write(term, indent + offset, index != 0)
        writer<<indent+offset+"];"+line
      }
      case t:TokenType=>  {
        writer << s"type = ${t.name};"
      }
      case d:Datum=>{

        writer<< "type = R_DATUM;"+line

        writer << (indent+++) + "r_datnum = Datnum { "
        write(d.termType,indent+++)
        writer << (d match {
          case b: BooleanDatum => s" r_bool = ${b.value};"
          case n: NumberDatum => s" r_num = ${n.value};"
          case s: StringDatum => " r_str = \""+s.value+"\";"
          case no:NoneDatum =>""

        })
        writer<<" };"+line

    }
      case t:Term=>{
        write(t.termType, indent +++)
        writer <<   line
        write(t.args, indent +++)
        write(t.optargs,indent )


        writer << indent+offset + "}"
      }
    /*token match {
      case t: Term => {

        val offset = writer << s"$name {" + line
        write(t.value.termType, indent + 2)
        write(t.value.args, indent + 2)
        writer << Indent(offset) + "}"

      }

      case du: Datum => {

        writer << indent + "r_datnum = Datnum { "
        writer << s"type = ${du.value.termType.name()}; "
        du.value match {
          case b: BooleanDatum => s"r_bool = ${b.value};"
          case n: NumberDatum => s"r_num = ${n.value};"
          case s: StringDatum => s"r_str = ${s.value};"
        }
        writer << "  };" + line
      }
      case dt:DatumType => {
        writer << s"type = ${dt.value.name()};" + line

      }
      case tt: TermType => {
        writer << s"type = ${tt.value.name()};" + line
      }
      case args: Args => {
        val offset = writer << "args = ["
        for ((term, index) <- args.args.zipWithIndex) write(term, indent + offset, index != 0)
      }  */
      case _=> writer <<"_blank_"
    }
    if(isTermBlock) writer<<(indent+++)+"}"
  }
}







