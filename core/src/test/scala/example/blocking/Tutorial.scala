package example.blocking

// 10 minute ReThink Db tutorial for Scala Driver
// http://rethinkdb.com/docs/guide/

import com.rethinkscala.Blocking._
import com.rethinkscala.Document

import org.scalatest._
import org.scalatest.junit.JUnitRunner

import org.junit.runner.RunWith

import scala.util._

case class Post(title: String, content: String)

case class Author(
  name: String,
  tvShow: String,
  posts: List[Post]
) extends Document

object Author {
  val authos = List(
    Author(
      "William Adama",
      "Battlestar Galactica",
      List(
        Post( "Decommissioning speech", "The Cylon War is long over..." ),
        Post( "We are at war", "Moments ago, this ship received..." ),
        Post( "The new Earth", "The discoveries of the past few days..." )
      )
    ),
    
    Author(
      "Laura Roslin",
      "Battlestar Galactica",
      List(
        Post( "The oath of office", "I, Laura Roslin, ..." ),
        Post( "They look like us", "The Cylons have the ability..." )
      )
    ),
    
    Author(
      "Jean-Luc Picard",
      "Star Trek TNG",
      List(
        Post( "Civil rights", "There are some words I've known since..." )    
      )
    )
  )
}

@RunWith(classOf[JUnitRunner])
class Tutorial extends FunSuite with Matchers with BeforeAndAfterAll {
  
  import functional._
  
  import com.rethinkscala.ast.Table
  
  var table: Table[Author] = _
  
  implicit var connection: BlockingConnection = _
  
  var id: String = _
  
  override def beforeAll {
    val version = Version3() // assumes rethink db runs in localhost and listens to the default driver port
    connection = Blocking(version)
    
    table = r.tableAs[Author]("authos")
  } 
  
  test ("the driver can create table") {
    table.create.run should equal (Success(true))
  }
  
  test("the driver can insert table with rows") {
    val insertion = 
      table.insert(Author.authos).run
    
    insertion.map(_.inserted) should equal (Success(3))  
    id = insertion.get.generatedKeys.head
  }
  
  test("the driver can iterate table") {
    println("==> iterable table")
    table.run.get foreach { author => println("==> " + author) }
    println("=" * 8)
  }
  
  test("the driver can filter documents based on a condition") {
    val matched = table.filter(_ \ "name" === "William Adama").run
    matched.map(_.size) should equal (Success(1))
    matched.map(_.head.name) should equal (Success("William Adama"))
  }
  
  test("the driver can filter documents based on another condition") {
    val matched = table.filter(author => (author \ "posts").count() > 2).run
    matched.map(_.size) should equal (Success(1))
    matched.map(_.head.posts.size > 2) should equal (Success(true))
  }
  
  test("the driver can retrieve document by primary key") {
    println("==> document retreived by primary key")
    println("==> " + table.get(id).run.get)
  }
  
  test("the driver can subscribe to real time feeds") {
    // not implemented yet
  }
  
  test("the driver can update documents with addition field") {
    table.update(Map("type" -> "functional")).run.map(_.replaced) should equal (Success(3))
    
    for {
      cursor <- table.run
      document <- cursor
    } {
      (document \ "type").as[String] should equal (Some("functional"))
    }
  }
  
  test("the driver can filter and then add field") {
    table
      .filter(_ \ "name" === "William Adama")
      .update(Map("rank" -> "Admiral"))
      .run.map(_.replaced) should equal (Success(1))
    
    table
      .filter(_ \ "name" === "William Adama")
      .run.map(author => (author.head \ "rank").as[String]) should equal (Success(Some("Admiral")))
  }
  
  test("the driver can filter and then enrich contents") {
    val post = Post("Shakespeare", "What a piece of work is man...")
    
    table
      .filter(_ \ "name" === "Jean-Luc Picard")
      .update(author => Map("posts" -> (author \ "posts").append(post)))
      .run.map(_.replaced) should equal (Success(1))
    
    table
      .filter(_ \ "name" === "Jean-Luc Picard")
      .run.map(_.head.posts.last) should equal (Success(post))
  }
  
  test("the driver can filter and then delete") {
    table
      .filter(author => (author \ "posts").count() < 3)
      .delete().run.map(_.deleted) should equal (Success(2))
    
    table
      .filter(author => (author \ "posts").count() < 3)
      .count().run should equal (Success(0))
  }
  
  test("the driver can drop table") {
    table.drop.run should equal (Success(true))
  }
}