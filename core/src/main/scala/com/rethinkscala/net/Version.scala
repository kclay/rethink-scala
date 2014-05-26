package com.rethinkscala.net

import org.jboss.netty.channel.Channel
import ql2.Ql2.{Query, VersionDummy}
import org.jboss.netty.handler.queue.{BlockingReadTimeoutException, BlockingReadHandler}
import org.jboss.netty.buffer.ChannelBuffer
import java.util.concurrent.{Executors, TimeUnit}
import java.io.IOException
import java.nio.charset.Charset
import ql2.{Ql2 => ql2}
import scala.concurrent.ExecutionContext
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.beans.BeanProperty
import com.rethinkscala.{DatumAssocPair, TermAssocPair, Term}
import com.rethinkscala.ast.{Datum, WithDB, DB}
import org.jboss.netty.buffer.ChannelBuffers._
import java.nio.ByteOrder
import scala.Some


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/3/13
 * Time: 5:22 PM
 *
 */

trait CompiledAst

case class ProtoBufCompiledAst(underlying: ql2.Term) extends CompiledAst

trait CompiledQuery {

  protected final def newBuffer(size: Int) = buffer(ByteOrder.LITTLE_ENDIAN, size)

  def encode: ChannelBuffer
}

class ProtoBufCompiledQuery(underlying: Query) extends CompiledQuery {
  override def encode = {
    val size = underlying.getSerializedSize
    val b = newBuffer(size + 4)
    b.writeInt(size)

    b.writeBytes(underlying.toByteArray)
    b
  }
}

class JsonCompiledQuery(underlying: String) extends CompiledQuery {
  override def encode = ???
}

abstract class Version extends LazyLogging {

  val host: String
  val port: Int
  val maxConnections: Int
  val db: Option[String]
  val timeout: Int = 10


  val executionContext: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))

  def configure(c: Channel)

  def toAst(term: Term): CompiledAst

  def toQuery(term: Term, token: Int, db: Option[String] = None, opts: Map[String, Any] = Map()): CompiledQuery
}


trait ProvidesProtoBufQuery {

  import scala.collection.JavaConversions.{seqAsJavaList, asJavaCollection}

  def toAst(term: Term): CompiledAst = ProtoBufCompiledAst(ast(term))

  def build(ta: TermAssocPair) = ql2.Term.AssocPair.newBuilder.setKey(ta.key).setVal(ast(ta.token)).build()

  def build(da: DatumAssocPair) = ql2.Datum.AssocPair.newBuilder.setKey(da.key).setVal(da.token.asInstanceOf[Datum].toMessage).build()

  private[this] def ast(term: Term): ql2.Term = {

    val opts = term.optargs.map {
      case ta: TermAssocPair => build(ta)
      // case da:DatumAssocPair=> build(da)
    }


    term.newBuilder.setType(term.termType)
      .addAllArgs(term.args.map(ast))
      .addAllOptargs(opts).build()
  }


  def toQuery(term: Term, token: Int, db: Option[String], opts: Map[String, Any]) = {

    def scopeDB(q: Query.Builder, db: DB) = q.addGlobalOptargs(Query.AssocPair.newBuilder.setKey("db").setVal(ast(db)))

    val query = Some(
      Query.newBuilder().setType(Query.QueryType.START)
        .setQuery(ast(term)).setToken(token).setAcceptsRJson(true)

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

    new ProtoBufCompiledQuery(query.build())

  }
}

case class Version1(host: String = "localhost", port: Int = 28015, db: Option[String] = None, maxConnections: Int = 5)
  extends Version with ProvidesProtoBufQuery {


  def configure(c: Channel) {
    c.write(VersionDummy.Version.V0_1).await()
  }


}

object Version1 {
  val builder = new Builder {

    def build = Version1(host, port, Option(db), maxConnections)
  }
}

abstract class Builder {

  @BeanProperty
  var host: String = "localhost"
  @BeanProperty
  var port: Int = 2801
  @BeanProperty
  var maxConnections: Int = 5
  @BeanProperty
  var db: String = ""
  @BeanProperty
  var timeout: Int = 10

  def build: Version

}

object Version2 {
  val builder = new Builder {
    @BeanProperty
    val authKey = ""

    def build = Version2(host, port, Option(db), maxConnections, authKey)
  }
}


case class Version2(host: String = "localhost", port: Int = 28015,
                    db: Option[String] = None, maxConnections: Int = 5,
                    authKey: String = "") extends Version with ProvidesProtoBufQuery {


  private[this] val AUTH_RESPONSE = "SUCCESS"

  def configure(c: Channel) {

    logger.debug("Configuring channel")
    val pipeline = c.getPipeline
    val authHandler = new BlockingReadHandler[ChannelBuffer]()
    pipeline.addFirst("authHandler", authHandler)


    c.write(VersionDummy.Version.V0_2)
    c.write(authKey).await()


    try {
      val response = Option(authHandler.read(timeout, TimeUnit.SECONDS)).map(b => b.toString(Charset.forName("US-ASCII"))).getOrElse("")

      logger.debug(s"Server auth responsed with : $response")

      if (!response.startsWith(AUTH_RESPONSE))
        throw new RethinkDriverError(s"Server dropped connection with message: '$response'")


    } catch {
      case e: BlockingReadTimeoutException => logger.error("Timeout error", e)
      case e: IOException => logger.error("Unable to read from socket", e)
    } finally {
      pipeline.remove(authHandler)
    }


  }
}
sealed abstract class Protocol

object ProtoBuf extends Protocol

object JSON extends Protocol

 class Version3(host: String = "localhost", port: Int = 28015,
                db: Option[String] = None, maxConnections: Int = 5,
                authKey: String = "",protocol:Protocol) extends Version2(host,port,db,maxConnections,authKey){


 }