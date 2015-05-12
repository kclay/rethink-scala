package example.async

import com.rethinkscala.Async._

import example.blocking.{ Author, Post }

import org.scalatest._
import org.scalatest.junit._

import org.junit.runner._

import scala.concurrent._
import scala.concurrent.duration._

@RunWith(classOf[JUnitRunner])
class Tutorial extends WordSpec with BeforeAndAfterAll with Matchers {
  
  import com.rethinkscala.ast.Table
  
  implicit var connection: AsyncConnection = _
  
  var table: Table[Author] = _
  var id: String = _
  
  override def beforeAll {
    val version = Version3()
    connection = Async(version)
    
    table = r.db("test").table[Author]("authors")
  }
  
  "async driver" should {
    "create table" in {
      val result = table.create.run
      Await.result(result, Duration.Inf) should equal (true)
    }
    
    "insert rows to table" in {
      val result = table.insert(Author.authos).run
      val inserted = Await.result(result, Duration.Inf)
      inserted.inserted should equal (3)
      
      id = inserted.generatedKeys.head
      
      val count = table.count().run
      Await.result(count, Duration.Inf) should equal (3)
    }
    
    "iterate rows in table" in {
      val result = table.run
      
      Await.result(result, Duration.Inf) foreach { author => println(">>> author: " + author) }
    }
    
    "filter table" in {
      val result = table.filter(_ \ "name" === "William Adama").run
      val matched = Await.result(result, Duration.Inf)
      
      matched.headOption.map(_.name) should equal (Some("William Adama"))
    }
    
    "filter table 2" in {
      val result = table.filter(author => (author \ "posts").count() > 2).run
      val matched = Await.result(result, Duration.Inf)
      
      matched.head.posts.size should be > 2
    }
    
    "retrieve row by primary key" in {
      val result = table.get(id).run
      val author = Await.result(result, Duration.Inf)
      
      Console.println(s">>> row by primrary key $id: $author")
    }
    
    "subscribe to real time feeds" in {
      // not supported in current implementation
    }
    
    "update field" in {
      val result = table.update(Map("type" -> "functional")).run
      Await.result(result, Duration.Inf).replaced should equal (3)
     
      Await.result(table.run, Duration.Inf) foreach { author => 
        (author \ "type").as[String] should equal (Some("functional"))
      }
    }
    
    "update based on filter result" in {
      val result = 
        table
          .filter(_ \ "name" === "William Adama")
          .update(Map("rank" -> "Admiral"))
          .run
          
      Await.result(result, Duration.Inf).replaced should equal (1)
      
      val verify = 
        table
          .filter(_ \ "name" === "William Adama")
          .run
          
      (Await.result(verify, Duration.Inf).head \ "rank").as[String] should equal (Some("Admiral"))
    }
    
    "enrich based on filter result" in {
      val post = Post("Shakespeare", "What a piece of work is man...")
      
      val result = 
        table
          .filter(_ \ "name" === "Jean-Luc Picard")
          .update(author => Map("posts" -> (author \ "posts").append(post)))
          .run
          
      Await.result(result, Duration.Inf).replaced should equal (1)
      
      val verify = 
        table
          .filter(_ \ "name" === "Jean-Luc Picard")
          .pluck("posts").run
          
      val verified = Await.result(verify, Duration.Inf).head("posts").asInstanceOf[List[Map[String, String]]].last
          
      verified("content") should equal (post.content)
      verified("title") should equal (post.title)
    }
    
    "delete based on filter" in {
      val result = table
        .filter(author => (author \ "posts").count() < 3)
        .delete.run
          
      Await.result(result, Duration.Inf)  
        
      val verify = table
        .filter(author => (author \ "posts").count() < 3)
        .count.run 
        
      Await.result(verify, Duration.Inf) should equal (0)
        
    }
    
    "drop table" in {
      val result = table.drop.run
      Await.result(result, Duration.Inf) should equal (true)
    }
  }
}