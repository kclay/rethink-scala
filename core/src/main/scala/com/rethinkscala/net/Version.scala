package com.rethinkscala.net

import org.jboss.netty.channel.{MessageEvent, ChannelHandlerContext, SimpleChannelUpstreamHandler, Channel}
import ql2.Ql2.VersionDummy
import org.jboss.netty.handler.queue.{BlockingReadTimeoutException, BlockingReadHandler}
import org.jboss.netty.buffer.{ChannelBufferIndexFinder, ChannelBuffer}
import java.util.concurrent.TimeUnit
import java.io.IOException
import java.nio.charset.Charset
import scala.concurrent.ExecutionContext


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 7/3/13
 * Time: 5:22 PM
 *
 */
abstract class Version {

  val host: String
  val port: Int
  val maxConnections: Int
  val db: Option[String]
  val timeout: Int = 10

  val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  def configure(c: Channel)
}

case class Version1(host: String = "localhost", port: Int = 28015, db: Option[String] = None, maxConnections: Int = 5) extends Version {
  def configure(c: Channel) {
    c.write(VersionDummy.Version.V0_1).await()
  }

}

case class Version2(host: String = "localhost", port: Int = 28015, db: Option[String] = None, maxConnections: Int = 5, authKey: String = "") extends Version {


  private[this] val AUTH_RESPONSE = "SUCCESS"

  def configure(c: Channel) {

    val pipeline = c.getPipeline
    val authHandler = new BlockingReadHandler[ChannelBuffer]()
    pipeline.addFirst("authHandler", authHandler)


    c.write(VersionDummy.Version.V0_2)
    c.write(authKey).await()


    try {
      val response = Option(authHandler.read(timeout, TimeUnit.SECONDS)).map(b => b.toString(Charset.forName("US-ASCII"))).getOrElse("")

      if (!response.startsWith(AUTH_RESPONSE))
        throw new RethinkDriverError(s"Server dropped connection with message: '$response'")


      pipeline.remove(authHandler)


    } catch {
      case e: BlockingReadTimeoutException =>
      case e: IOException =>
    }


  }
}