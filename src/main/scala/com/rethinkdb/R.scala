package com.rethinkdb

import com.rethinkdb.ast.Table

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 5/28/13
 * Time: 5:53 PM
 * To change this template use File | Settings | File Templates.
 */
object r{
  def table(name:String,useOutDated:Option[Boolean]=None) = Table(name,useOutDated)
}