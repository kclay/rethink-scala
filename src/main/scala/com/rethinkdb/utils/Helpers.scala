package com.rethinkdb.utils

import com.rethinkdb.Term
import ql2.Query
import com.rethinkdb.ast.WithDB

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 5/26/13
 * Time: 9:38 PM
 * To change this template use File | Settings | File Templates.
 */
object Helpers {

  def toQuery(term: Term,token:Int)={
    val query = Some(
      Query().setType(Query.QueryType.START)
        .setQuery(term.toInternalTerm).setToken(token)

    ).map(q => {
      term match {
        case d: WithDB => {
          d.scopedDB.map(db=>q.addAllGlobalOptargs(Query.AssocPair(Some("db"),Some(db.toDatum.toInternalTerm)))).get

        }
        case _ => q
      }

    }).get
    query

  }
}
