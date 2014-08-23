package com.rethinkscala.net

import java.io.IOException
import java.nio.ByteOrder
import java.nio.charset.Charset
import java.util.concurrent.{Executors, TimeUnit}

import com.rethinkscala.ast._
import com.rethinkscala.reflect.Reflector
import com.rethinkscala.{Term, TermAssocPair}
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.jboss.netty.buffer.ChannelBuffer
import org.jboss.netty.buffer.ChannelBuffers._
import org.jboss.netty.channel.Channel
import org.jboss.netty.handler.queue.{BlockingReadHandler, BlockingReadTimeoutException}
import ql2.Ql2.{Response, Query, VersionDummy}
import ql2.{Ql2 => ql2}

import scala.beans.BeanProperty
import scala.concurrent.{Promise, ExecutionContext}


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/3/13
 * Time: 5:22 PM
 *
 */

trait CompiledAst

case class ProtoBufCompiledAst(underlying: ql2.Term) extends CompiledAst


trait JsonAst {

  type T

  def toValue: T

}

case class DatumJsonAst(datumType: ql2.Datum.DatumType, value: Any) extends JsonAst {

  type T = Any

  override def toValue = value
}

case class TermJsonAst(termType: ql2.Term.TermType, term: Seq[JsonAst], opts: Map[String, Any] = Map.empty)

  extends JsonAst {
  type T = Seq[Any]

  def toValue = if (opts.nonEmpty) Seq(termType.getNumber, term.map(_.toValue), opts)
  else Seq(termType.getNumber, term.map(_.toValue))
}

case class JsonCompiledAst(underlying: JsonAst) extends CompiledAst


trait CompiledQuery[T] {

  val token: Long

  protected final def newBuffer(size: Int) = buffer(ByteOrder.LITTLE_ENDIAN, size)

  def encode: ChannelBuffer

  type TokenType[R]

  def asToken(conn: Connection, term: Term, promise: Promise[T])(implicit mf: Manifest[T]): TokenType[T]


  def cursor = DefaultCursorFactory.apply[T] _
}

class ProtoBufCompiledQuery[T](underlying: Query) extends CompiledQuery {

  override val token: Long = underlying.getToken

  override def encode = {
    val size = underlying.getSerializedSize
    val b = newBuffer(size + 4)
    b.writeInt(size)

    b.writeBytes(underlying.toByteArray)
    b
  }

  override type TokenType[R] = QueryToken[R]

  //override def asToken(conn: Connection, term: Term, promise: Promise[T])(implicit mf: Manifest[T]) = QueryToken[T](conn, this, term, promise)
  override def asToken(conn: Connection, term: Term, promise: Promise[T])(implicit mf: Manifest[T]) = QueryToken[T](conn, this, term, promise)
}

case class JsonQuery(queryType: ql2.Query.QueryType, ast: JsonAst, opts: Map[String, Any]) {
  def toSeq = Seq(queryType.getNumber, ast.toValue, opts)

}

case class JsonCompiledQuery[T](token: Long, query: JsonQuery) extends CompiledQuery {

  lazy val json = Reflector.toJson(query.toSeq)
  private val TOKEN_SIZE = 8

  // long
  override def encode = {
    val jsonBytes = json.getBytes
    val jsonSize = jsonBytes.length
    val size = TOKEN_SIZE + 4 + jsonSize // token + json len + json string
    val b = newBuffer(size)
    b.writeLong(token)
    b.writeInt(jsonSize)
    b.writeBytes(jsonBytes)
    b
  }


  override type TokenType[R] = JsonQueryToken[R]

  override def asToken[T](conn: Connection, term: Term, promise: Promise[T])(implicit mf: Manifest[T]) = JsonQueryToken[T](conn, this, term, promise)
}

abstract class Version extends LazyLogging {

  val host: String
  val port: Int
  val maxConnections: Int
  val db: Option[String]
  val timeout: Int = 10

  private[rethinkscala] val pipelineFactory: RethinkPipelineFactory


  val executionContext: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))

  type ResponseType

  def newHandler: VersionHandler[ResponseType]

  def configure(c: Channel)

  def toAst(term: Term): CompiledAst

  def toQuery[T](term: Term, token: Long, db: Option[String] = None, opts: Map[String, Any] = Map()): CompiledQuery[T]
}


trait ProvidesQuery {

  type Builder
  type Query[Q] <: CompiledQuery[Q]
  type Ast <: CompiledAst

  def newQueryBuilder(queryType: ql2.Query.QueryType, token: Long, opts: Map[String, Any]): Builder

  def withDB(builder: Builder, db: DB): Builder

  def toAst(term: Term): Ast

  def compile[T](builder: Builder, term: Term): Query[T]


  def toQuery[T](term: Term, token: Long, db: Option[String], opts: Map[String, Any]): Query[T] = {


    val builder = newQueryBuilder(Query.QueryType.START, token, opts)


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



    compile[T](finalBuilder, term)
  }
}

trait ProvidesProtoBufQuery extends ProvidesQuery {

  import scala.collection.JavaConversions.{asJavaCollection, seqAsJavaList}

  override type Query = ProtoBufCompiledQuery
  override type Ast = ProtoBufCompiledAst

  def toAst(term: Term) = ProtoBufCompiledAst(ast(term))

  def build(ta: TermAssocPair) = ql2.Term.AssocPair.newBuilder.setKey(ta.key).setVal(ast(ta.token)).build()


  // def build(da: TermAssocPair) = ql2.Datum.AssocPair.newBuilder.setKey(da.key).setVal(da.token.asInstanceOf[Datum].toMessage).build()


  type Builder = Query.Builder

  private[this] def ast(term: Term): ql2.Term = {

    val opts = term.optargs.map {
      case ta: TermAssocPair => build(ta)
      // case da:DatumAssocPair=> build(da)
    }





    val builder = term match {
      case d: Datum => {
        val db = ql2.Datum.newBuilder.setType(d.datumType)
        d match {
          case s: StringDatum => db.setRStr(s.value)
          case b: BooleanDatum => db.setRBool(b.value)
          case n: NoneDatum =>
          case n: NumberDatum => db.setRNum(n.value)
        }
        ql2.Term.newBuilder().setDatum(db.build())


      }
      case _ => ql2.Term.newBuilder()
    }



    builder.setType(term.termType)
      .addAllArgs(term.args.map(ast))
      .addAllOptargs(opts).build()
  }


  def newQueryBuilder(queryType: ql2.Query.QueryType, token: Long, opts: Map[String, Any]): Builder = {
    val q = Query.newBuilder().setType(Query.QueryType.START).setToken(token).setAcceptsRJson(true)
    opts.foreach {
      case (key, value) => q.addGlobalOptargs(ql2.Query.AssocPair.newBuilder.setKey(key).setVal(ast(Expr(value))))
    }
    q


  }

  def withDB(q: Builder, db: DB) = q.addGlobalOptargs(Query.AssocPair.newBuilder.setKey("db").setVal(ast(db)))

  def compile(builder: Builder, term: Term) = new ProtoBufCompiledQuery(builder.setQuery(ast(term)).build())
}

/*
@deprecated("Use Version2 or Version3")
case class Version1(host: String = "localhost", port: Int = 28015, db: Option[String] = None, maxConnections: Int = 5)
  extends Version with ProvidesProtoBufQuery {


  override type ResponseType = ql2.Response

  override def newHandler = ???

  override private[rethinkscala] val pipelineFactory: RethinkPipelineFactory = ProtoPipelineFactory

  def configure(c: Channel) {
    c.write(VersionDummy.Version.V0_1).await()
  }


}        */


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


trait ConfigureAuth {
  self: Version =>

  val authKey: String

  private[this] val AUTH_RESPONSE = "SUCCESS"

  val version: VersionDummy.Version = VersionDummy.Version.V0_2

  protected def write(c: Channel): Unit = {
    c.write(version)
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

      logger.debug(s"Server auth responsed with : -$response-")

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
                    authKey: String = "") extends Version with ProvidesProtoBufQuery with ConfigureAuth {
  override private[rethinkscala] val pipelineFactory: RethinkPipelineFactory = ProtoPipelineFactory
  override type ResponseType = ql2.Response

  override def newHandler = ???
}

sealed abstract class Protocol {
  type Query <: CompiledQuery
  type Ast <: CompiledAst
  val value: ql2.VersionDummy.Protocol

  def toAst(term: Term): Ast

  def toQuery[T](term: Term, token: Long, db: Option[String], opts: Map[String, Any]): Query

}

object ProtoBuf extends Protocol with ProvidesProtoBufQuery {

  override val value = ql2.VersionDummy.Protocol.PROTOBUF
}


trait ProvidesJsonQuery extends ProvidesQuery {

  override type Query[Q] = JsonCompiledQuery[Q]
  override type Ast = JsonCompiledAst

  case class QueryBuilder(queryType: ql2.Query.QueryType, token: Long, opts: Map[String, Any] = Map.empty)

  override type Builder = QueryBuilder

  override def withDB(builder: Builder, db: DB) = builder.copy(opts = (builder.opts /: Map("db" -> db.name))(_ + _))

  override def newQueryBuilder(queryType: ql2.Query.QueryType, token: Long, opts: Map[String, Any]) =
    QueryBuilder(queryType, token, opts)

  override def compile[T](builder: Builder, term: Term) = new JsonCompiledQuery[T](builder.token, JsonQuery(builder.queryType, ast(term), builder.opts))


  override def toAst(term: Term) = JsonCompiledAst(ast(term))


  private[this] def ast(term: Term): JsonAst = {

    val opts = term match {

      case MakeObj2(doc) => Reflector.fields(doc).map(f =>
        (f.getName, f.get(doc))).toMap
      case MakeObj(data) => data
      case _ => term.optargs.map {
        case ta: TermAssocPair => ta.key -> ast(ta.token)
        // case da:DatumAssocPair=> build(da)
      }.toMap

    }

    term match {
      case d: Datum => DatumJsonAst(d.datumType, d.value)
      case _ => TermJsonAst(term.termType, term match {
        case i: Insert[_, _] => i.argsForJson.map(ast)

        case _ => term.args.map(ast)
      }, opts)
    }


  }

}

object JSON extends Protocol with ProvidesJsonQuery {

  override val value = ql2.VersionDummy.Protocol.JSON
}


case class Version3(host: String = "localhost", port: Int = 28015,
                    db: Option[String] = None, maxConnections: Int = 5,
                    authKey: String = "", protocol: Protocol = JSON) extends Version with ConfigureAuth {

  override type ResponseType = String

  override def newHandler = new JsonVersionHandler(this)

  override private[rethinkscala] val pipelineFactory: RethinkPipelineFactory = JsonPipelineFactory
  override val version = VersionDummy.Version.V0_3

  override protected def write(c: Channel) = {
    super.write(c)
    c.write(protocol.value)

  }

  type Ast = JSON.Ast
  type Query[Q] = JSON.Query[Q]

  override def toAst(term: Term): Ast = JSON.toAst(term)

  override def toQuery[T](term: Term, token: Long, db: Option[String], opts: Map[String, Any]): Query[T] = JSON.toQuery[T](term, token, db, opts)
}

trait Versions {

  def Version2(host: String = "localhost", port: Int = 28015,
               db: Option[String] = None, maxConnections: Int = 5,
               authKey: String = "") = new Version2(host, port, db, maxConnections, authKey)
}