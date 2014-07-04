package com.rethinkscala.net

import com.rethinkscala.reflect.Reflector
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
import com.rethinkscala.ast.{Expr, Datum, WithDB, DB}
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

case class JsonCompiledAst(underlying:Seq[Any]) extends CompiledAst

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

class JsonCompiledQuery(token:Long,seq:Seq[Any]) extends CompiledQuery {
  lazy val json = Reflector.toJson(seq)
  private val TOKEN_SIZE = 8 // long
  override def encode = {
    val jsonBytes  = json.getBytes
   val size = TOKEN_SIZE +  4 +jsonBytes.length // token + json len + json string
    val b = newBuffer(size)
    b.writeLong(token)
    b.writeInt(size)
    b.writeBytes(jsonBytes)
    b
  }
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

  def toQuery(term: Term, token: Long, db: Option[String] = None, opts: Map[String, Any] = Map()): CompiledQuery
}


trait ProvidesQuery{

  type Builder

  def newQueryBuilder(queryType:ql2.Query.QueryType,token:Long,opts: Map[String, Any]):Builder

  def withDB(builder:Builder,db:DB):Builder

  def toAst(term: Term): CompiledAst
  def compile(builder:Builder,term:Term):CompiledQuery

  def toQuery(term: Term, token: Long, db: Option[String], opts: Map[String, Any]):CompiledQuery={



    val builder = newQueryBuilder(Query.QueryType.START,token,opts)


     val finalBuilder = opts.get("db").map {
        case name: String => withDB(builder, DB(name))
      }.getOrElse {
        term match {
          case d: WithDB => d.db.map(withDB(builder, _)).getOrElse(db.map {
            name => withDB(builder, DB(name))
          }.getOrElse(builder))
          case _ => db.map {
            name => withDB(builder, DB(name))
          }.getOrElse(builder)
        }

      }



    compile(finalBuilder,term)
  }
}

trait ProvidesProtoBufQuery extends ProvidesQuery{

  import scala.collection.JavaConversions.{seqAsJavaList, asJavaCollection}

  def toAst(term: Term): CompiledAst = ProtoBufCompiledAst(ast(term))

  def build(ta: TermAssocPair) = ql2.Term.AssocPair.newBuilder.setKey(ta.key).setVal(ast(ta.token)).build()

  def build(da: DatumAssocPair) = ql2.Datum.AssocPair.newBuilder.setKey(da.key).setVal(da.token.asInstanceOf[Datum].toMessage).build()

  type Builder = Query.Builder
  private[this] def ast(term: Term): ql2.Term = {

    val opts = term.optargs.map {
      case ta: TermAssocPair => build(ta)
      // case da:DatumAssocPair=> build(da)
    }


    term.newBuilder.setType(term.termType)
      .addAllArgs(term.args.map(ast))
      .addAllOptargs(opts).build()
  }



  def newQueryBuilder(queryType:ql2.Query.QueryType,token:Long,opts: Map[String, Any]):Builder={
    val q = Query.newBuilder().setType(Query.QueryType.START).setToken(token).setAcceptsRJson(true)
    opts.foreach{
      case (key,value)=> q.addGlobalOptargs( ql2.Query.AssocPair.newBuilder.setKey(key).setVal(ast(Expr(value))))
    }
    q


  }
  def withDB(q: Builder, db: DB) = q.addGlobalOptargs(Query.AssocPair.newBuilder.setKey("db").setVal(ast(db)))
  def compile(builder:Builder,term:Term) =  new ProtoBufCompiledQuery(builder.setQuery(ast(term)).build())
}

case class Version1(host: String = "localhost", port: Int = 28015, db: Option[String] = None, maxConnections: Int = 5)
  extends Version with ProvidesProtoBufQuery {


  def configure(c: Channel) {
    c.write(VersionDummy.Version.V0_1).await()
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



trait ConfigureAuth{
  self:Version=>

  val authKey:String

  private[this] val AUTH_RESPONSE = "SUCCESS"

  protected def write(c:Channel):Unit={
    c.write(VersionDummy.Version.V0_2)
    c.write(authKey).await()
  }
  def configure(c: Channel) {

    logger.debug("Configuring channel")
    val pipeline = c.getPipeline
    val authHandler = new BlockingReadHandler[ChannelBuffer]()
    pipeline.addFirst("authHandler", authHandler)

    write(c)



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
case class Version2(host: String = "localhost", port: Int = 28015,
                    db: Option[String] = None, maxConnections: Int = 5,
                    authKey: String = "") extends Version with ProvidesProtoBufQuery with ConfigureAuth
sealed abstract class Protocol{
  type Query<: CompiledQuery
  type Ast <:CompiledAst
  val value:ql2.VersionDummy.Protocol
  def toAst(term:Term):Ast
  def toQuery(term: Term, token: Long, db: Option[String], opts: Map[String, Any]):Query

}

object ProtoBuf extends Protocol with ProvidesProtoBufQuery{

  override type Query = ProtoBufCompiledQuery
  override type Ast = ProtoBufCompiledAst
  override val value= ql2.VersionDummy.Protocol.PROTOBUF
}


trait ProvidesJsonQuery extends ProvidesQuery{

  case class QueryBuilder(queryType:ql2.Query.QueryType,token:Long,opts:Map[String,Any] = Map.empty)
  override type Builder = QueryBuilder

  override def withDB(builder: Builder, db: DB) = builder.copy(opts=(builder.opts/:Map("db"->db))(_+_))

  override def newQueryBuilder(queryType:ql2.Query.QueryType, token: Long,opts:Map[String,Any]) = QueryBuilder(queryType,token,opts)

  override def compile(builder: Builder,term:Term) = new JsonCompiledQuery(builder.token,Seq(builder.queryType,ast(term),builder.opts))



  override def toAst(term: Term) = JsonCompiledAst(ast(term))


  private[this] def ast(term: Term):Seq[Any]= {

    val opts = term.optargs.map {
      case ta: TermAssocPair => ta.key-> ast(ta.token)
      // case da:DatumAssocPair=> build(da)
    }.toMap

    Seq(term.termType,term.args.map(ast),opts)

  }

}
object JSON extends Protocol with ProvidesJsonQuery{
  override val value= ql2.VersionDummy.Protocol.JSON
}


case class Version3(host: String = "localhost", port: Int = 28015,
                db: Option[String] = None, maxConnections: Int = 5,
                authKey: String = "",protocol:Protocol) extends Version with ConfigureAuth{
  override protected def write(c: Channel) = {
    super.write(c)

  }
  type Ast  = protocol.Ast
  type Query = protocol.Query

  override def toAst(term: Term):Ast = protocol.toAst(term)

  override def toQuery(term: Term, token: Int, db: Option[String], opts: Map[String, Any]):Query = protocol.toQuery(term,token,db,opts)
}