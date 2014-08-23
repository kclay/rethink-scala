package com.rethinkscala.net

import java.io.IOException
import java.nio.ByteOrder
import java.nio.charset.Charset
import java.util.concurrent.{Executors, TimeUnit}

import com.rethinkscala.ast._
import com.rethinkscala.reflect.Reflector
import com.rethinkscala.{ResultExtractor, Term, TermAssocPair}
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.jboss.netty.buffer.ChannelBuffer
import org.jboss.netty.buffer.ChannelBuffers._
import org.jboss.netty.channel.Channel
import org.jboss.netty.handler.queue.{BlockingReadHandler, BlockingReadTimeoutException}
import ql2.Ql2.{Query, VersionDummy}
import ql2.{Ql2 => ql2}

import scala.beans.BeanProperty
import scala.concurrent.{ExecutionContext, Promise}


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



  def toValue: Any

}

case class DatumJsonAst(datumType: ql2.Datum.DatumType, value: Any) extends JsonAst {



  override def toValue = value
}


case class TermJsonAst(termType: ql2.Term.TermType, term: Seq[JsonAst], opts: Map[String, Any] = Map.empty)

  extends JsonAst {

  def options= opts.mapValues {
    case v: JsonAst => v.toValue
    case a: Any => a
  }

  def toValue = termType match {
    case ql2.Term.TermType.MAKE_OBJ =>options

    case _ => if
    (opts.nonEmpty) Seq(termType.getNumber, term.map(_.toValue), options)
    else Seq(termType.getNumber, term.map(_.toValue))
  }
}


case class JsonCompiledAst(underlying: JsonAst) extends CompiledAst


trait CompiledQuery {

  type Result
  val token: Long

  protected final def newBuffer(size: Int) = buffer(ByteOrder.LITTLE_ENDIAN, size)

  def encode: ChannelBuffer

  type TokenType[R]

  def asToken(conn: Connection, term: Term, promise: Promise[Result])(implicit extractor: ResultExtractor[Result]): TokenType[Result]


  //private val _cursor:RethinkCursor[Result] = None
  def cursor(factory: CursorFactory) = factory.apply[Result] _
}

class ProtoBufCompiledQuery[T](underlying: Query) extends CompiledQuery {


  override type Result = T

  override def asToken(conn: Connection, term: Term, promise: Promise[Result])(implicit extractor: ResultExtractor[Result]) = QueryToken[Result](conn, this, term, promise)

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

}

case class JsonQuery(queryType: ql2.Query.QueryType, ast: JsonAst, opts: Map[String, Any]) {
  def toSeq = Seq(queryType.getNumber, ast.toValue, opts)

}

case class JsonCompiledQuery[T](token: Long, query: JsonQuery) extends CompiledQuery {


  override type Result = T

  override def asToken(conn: Connection, term: Term, promise: Promise[Result])(implicit extractor: ResultExtractor[Result]) = JsonQueryToken[Result](conn, this, term, promise)

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

  type Query[T] <: CompiledQuery {type Result = T}
  type Ast

  def toQuery[T](term: Term, token: Long, db: Option[String] = None, opts: Map[String, Any] = Map()): Query[T]
}


trait ProvidesQuery {
  self: Version =>
  type Builder

  type Ast = CompiledAst

  def newQueryBuilder(queryType: ql2.Query.QueryType, token: Long, opts: Map[String, Any]): Builder

  def withDB(builder: Builder, db: DB): Builder

  def toAst(term: Term): Ast

  def compile[T](builder: Builder, term: Term): Query[T]


  def toQuery[T](term: Term, token: Long, db: Option[String], opts: Map[String, Any]) = {


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

  self: Version =>

  import scala.collection.JavaConversions.{asJavaCollection, seqAsJavaList}


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


  override type Query[T] = ProtoBufCompiledQuery[T]

  def withDB(q: Builder, db: DB) = q.addGlobalOptargs(Query.AssocPair.newBuilder.setKey("db").setVal(ast(db)))

  def compile[T](builder: Builder, term: Term) = new ProtoBufCompiledQuery[T](builder.setQuery(ast(term)).build())
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

@deprecated("Use Version3")
case class Version2(host: String = "localhost", port: Int = 28015,
                    db: Option[String] = None, maxConnections: Int = 5,
                    authKey: String = "") extends Version with ProvidesProtoBufQuery with ConfigureAuth {
  override private[rethinkscala] val pipelineFactory: RethinkPipelineFactory = ProtoPipelineFactory
  override type ResponseType = ql2.Response

  override def newHandler = new ProtoVersionHandler(this)
}


trait ProvidesJsonQuery extends ProvidesQuery {

  self: Version =>

  override type Query[T] = JsonCompiledQuery[T]

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

      case _ => term.optargs.map {
        case ta: TermAssocPair => {
          if(ta.token.isInstanceOf[Order]){
            val a = ""
          }
          ta.key -> ast(ta.token)
        }
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


case class Version3(host: String = "localhost", port: Int = 28015,
                    db: Option[String] = None, maxConnections: Int = 5,
                    authKey: String = "") extends Version with ConfigureAuth with ProvidesJsonQuery {

  override type ResponseType = String

  override def newHandler = new JsonVersionHandler(this)

  override private[rethinkscala] val pipelineFactory: RethinkPipelineFactory = JsonPipelineFactory
  override val version = VersionDummy.Version.V0_3

  override protected def write(c: Channel) = {
    super.write(c)
    c.write(ql2.VersionDummy.Protocol.JSON)

  }


}

trait Versions {

  def Version2(host: String = "localhost", port: Int = 28015,
               db: Option[String] = None, maxConnections: Int = 5,
               authKey: String = "") = new Version2(host, port, db, maxConnections, authKey)
}