package com.rethinkdb.utils


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/20/13
 * Time: 8:29 PM 
 */

import com.rethinkdb.Ast
import ql2.{Ql2 => p}
import com.rethinkdb.Ast.{StringDatum, NumberDatum, BooleanDatum}

trait Writer {
  def <<(value: String): Int = write(value)

  def write(value: String): Int
}

class StringBufferTermWriter extends Writer {
  private val builder = StringBuilder.newBuilder

  def write(value: String) {
    builder + value
    value.length

  }
}

class Tree(term: Ast.Term) {

  val line = "\n"

  sealed abstract class Leaf

  case object Line extends Leaf

  case class Text(s: String)

  case class Term(value: Ast.Term) extends Leaf

  case class TermType(value: p.Term.TermType) extends Leaf

  case class Datum(value: Ast.Datum) extends Leaf

  case class DatumType(value: p.Datum.DatumType) extends Leaf

  case class Args(args: Seq[Ast.Term]) extends Leaf

  case class Indent(amount: Int) {
    def +(add: Int) = copy(amount + add)

    def ++() = this + 1

    def -(remove: Int) = copy(amount - remove)

    def --() = this - 1
  }


  private val defaultIndent = Indent(0)

  implicit def indent2String(indent: Indent) = " " * indent.amount

  implicit def args2Args(args: Seq[Ast.Term]) = Args(args)

  implicit def termType2TermType(termType: p.Term.TermType) = TermType(termType)

  implicit def term2Term(term: Ast.Term) = Term(term)

  implicit def datum2Datum(value: p.Datum) = Datum(value)

  implicit def datumType2DatumType(value: p.Datum.DatumType) = DatumType(value)


  def >>(writer: Writer) {
    write(term)(writer)
  }

  def writeType

  private def write(leaf: Leaf, indent: Indent = defaultIndent, applyIndent: Boolean = true)(implicit writer: Writer) {
    writer << indent

    leaf match {
      case t: Term => {

        val offset = writer << "Term {" + line
        write(t.value.termType, indent + 2)
        write(t.value.args, indent + 2)
        writer << Indent(offset) + "}"

      }

      case du: Datum => {
        write(du.value.termType, indent + 2)
        writer << indent + "r_datnum = Datnum { "
        writer << s"type = ${du.value.termType.name()}; "
        du.value match {
          case b: BooleanDatum => s"r_bool = ${b.value};"
          case n: NumberDatum => s"r_num = ${n.value};"
          case s: StringDatum => s"r_str = ${s.value};"
        }
        writer << "  };" + line
      }
      case dt: DatumType => {
        writer << s"type = ${dt.value.name()};" + line

      }
      case tt: TermType => {
        writer << s"type = ${tt.value.name()};" + line
      }
      case args: Args => {
        val offset = writer << "args = ["
        for ((term, index) <- args.args.zipWithIndex) write(term, indent + offset, index != 0)
      }
    }
  }
}







