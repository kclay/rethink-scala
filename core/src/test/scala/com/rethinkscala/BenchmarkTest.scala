package com.rethinkscala

import org.scalatest.FunSuite
import scala.compat.Platform
import com.rethinkscala.net.Document

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/15/13
 * Time: 11:15 AM 
 */
class BenchmarkTest extends FunSuite with BaseTest {

  var multiplier = 1


  test("benchmarks") {

    for (i <- 0 to 12) {

    }


  }

  def bench(pass: Int, creator: (Int) => Either[Document, Seq[Map[String, Any]]]) {
    val d = r.db(randomAlphanumericString(5))
    val t = d.table[Document](randomAlphanumericString(5))
    d.create.run
    t.create.run
    val records = creator(pass)
    def around(f: () => Unit) = {
      val startTime = Platform.currentTime


      val stopTime = Platform.currentTime

      val total = (stopTime - startTime);
      Platform.collectGarbage
      println("Inserting " + Math.pow(2, pass)) + " rows took " + (total) + "ms and " + (total / 1000) + "s"
    }
    records match {
      case Left(x) => around(() => t.insert(x))
      case Right(y) => around(() => t.insert(y))
    }

  }

  /*
def runBenchmark(noTimes: Int)(f: () => Unit): List[Long] =
 for (i <- List.range(1, noTimes + 1)) yield {


   var i = 0; while (i < multiplier) {
     f()
     i += 1
   }




   stopTime - startTime
 }
        */

}
