package com.rethinkscala.utils

import ql2.Ql2.Query
import com.rethinkscala.ast.{DB, WithDB}
import com.rethinkscala.Term

/** Created with IntelliJ IDEA.
  * User: keyston
  * Date: 5/26/13
  * Time: 9:38 PM
  * To change this template use File | Settings | File Templates.
  */
object Helpers {

  def toQuery(term: Term, token: Int, db: Option[String] = None, opts: Map[String, Any] = Map()) = {

    def scopeDB(q: Query.Builder, db: DB) = q.addGlobalOptargs(Query.AssocPair.newBuilder.setKey("db").setVal(db.ast))

    val query = Some(
      Query.newBuilder().setType(Query.QueryType.START)
        .setQuery(term.ast).setToken(token)

    ).map(q => {

      opts.get("db").map {
        case name: String => scopeDB(q, DB(name))
      }.getOrElse {
        term match {
          case d: WithDB => d.db.map(scopeDB(q, _)).getOrElse(db.map {
            name => scopeDB(q, DB(name))
          }.getOrElse(q))
          case _ => db.map {
            name => scopeDB(q, DB(name))
          }.getOrElse(q)
        }

      }


    }).get
    query.build()

  }
}
