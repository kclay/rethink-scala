package com.rethinkscala.utils

import ql2.Query
import com.rethinkscala.ast.{ DB, WithDB }
import com.rethinkscala.Term

/** Created with IntelliJ IDEA.
 *  User: keyston
 *  Date: 5/26/13
 *  Time: 9:38 PM
 *  To change this template use File | Settings | File Templates.
 */
object Helpers {

  def toQuery(term: Term, token: Int, db: Option[String] = None) = {

    def scopeDB(q: Query, db: DB) = q.addAllGlobalOptargs(Query.AssocPair(Some("db"), Some(db.ast)))
    val query = Some(
      Query().setType(Query.QueryType.START)
        .setQuery(term.ast).setToken(token)

    ).map(q => {
        term match {
          case d: WithDB => {
            d.db.map(scopeDB(q, _)).getOrElse(db.map { name => scopeDB(q, DB(name)) }.getOrElse(q))

          }
          case _ => q


        }

      }).get
    query

  }
}
