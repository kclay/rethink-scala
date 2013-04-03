package com.rethinkdb.ast

import ql2.{Ql2=>p}



  sealed trait Token

  trait TermBlock extends Token

  trait TokenType extends Token {
    type T

    def name: String

    def value: T
  }

  case class TermTokenType(termType: p.Term.TermType) extends TokenType {
    type T = p.Term.TermType

    def name: String = termType.name

    def value = termType
  }

  case class DatumTokenType(datumType: p.Datum.DatumType) extends TokenType {
    type T = p.Datum.DatumType

    def name: String = datumType.name

    def value = datumType
  }

  trait AssocPairToken extends Token {


    val key: String
    val value: Any
    lazy val token = Expr(value)




    type T


    trait Builder {

      val get: Any
    }


    def pair: T


  }

  case class TermAssocPairToken(key: String, value: Any) extends AssocPairToken {

    type T = p.Term.AssocPair


    private lazy val builder = new Builder {

      val get = {
        val term = p.Term.newBuilder
        token.compile(term)
        p.Term.AssocPair.newBuilder.setKey(key).setVal(term.build).build
      }
    }

    def unwrap: p.Term.AssocPair = pair.asInstanceOf[p.Term.AssocPair]

    def pair: T = builder.get.asInstanceOf[T]


  }

  case class DatumAssocPairToken(key: String, value: Any) extends AssocPairToken {
    type T = p.Datum.AssocPair

    private lazy val builder = new Builder {
      val term = p.Term.newBuilder
      token.compile(term)

      val get = p.Datum.AssocPair.newBuilder.setKey(key).setVal(p.Datum.newBuilder.build).build
    }

    def unwrap: p.Datum.AssocPair = pair.asInstanceOf[p.Datum.AssocPair]

    def pair: T = builder.get.asInstanceOf[T]

  }

  case class Args(args: Iterable[Token]) extends Token

  case class OptArgs(optargs: Iterable[AssocPairToken]) extends Token

  // implicit def tokenTypeToTermType(tokenType:Either[p.Term.TermType,p.Datum.DatumType]):p.Term.TermType =tokenType.left


