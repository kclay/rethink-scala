package com.rethinkscala

import Blocking.functional._
import org.scalatest.FunSuite

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 4/9/2015
 * Time: 9:45 AM 
 */
case class Unicode(name: String)

class UnicodeTest extends FunSuite with WithBase {


  test("unicode") {

    val t = table.to[Unicode]

    val value = Unicode("L’Occitane Broadway")
    val afterInsert = t.insert(value)
      .run.flatMap(res => t.get(res.generatedKeys.head).run)
    println(value)
    println(afterInsert)


  }


  override val dropDB: Boolean = false
  override lazy val dbName: String = "unicode"
  override lazy val tableName: String = "unicode"

}
