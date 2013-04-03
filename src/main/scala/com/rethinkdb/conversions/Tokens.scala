package com.rethinkdb.conversions

import com.rethinkdb.ast.{DatumTokenType, TermTokenType,TokenType,Args,Token,AssocPairToken,OptArgs,DB}
import com.rethinkdb.Term
import ql2.{Ql2=>p}




object Tokens  {


    implicit def termType2TokenType(termType: p.Term.TermType): TermTokenType = TermTokenType(termType)

    implicit def termTokenType2TermType(token: TermTokenType): p.Term.TermType = token.termType

    implicit def datumType2TokenType(datumType: p.Datum.DatumType): DatumTokenType = DatumTokenType(datumType)

    implicit def datumTokenType2DatumType(token: DatumTokenType): p.Datum.DatumType = token.datumType

    implicit def tokenType2TokenType(tokenType: TokenType) = tokenType match {
    case dt: DatumTokenType => dt.datumType
    case tt: TermTokenType => tt.termType
    }

    implicit def args2Token(args: Iterable[Token]): Args = Args(args)

    implicit def optArgs2Token(optargs: Iterable[AssocPairToken]) = OptArgs(optargs)

    implicit def termBaseToTerm(base: Term): p.Term = {
    val builder = p.Term.newBuilder()
    base.compile(builder)
    builder.build()


    }

    implicit def optargToAssocPair(i: (String, Term)): p.Term.AssocPair = {

    p.Term.AssocPair.newBuilder().setKey(i._1).setVal(i._2).build()
    }

    implicit def dbToQueryAssocPair(db: DB): p.Query.AssocPair = {
    p.Query.AssocPair.newBuilder().setKey("db").setVal(db).build
    }
}