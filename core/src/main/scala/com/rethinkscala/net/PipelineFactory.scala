package com.rethinkscala.net

import org.jboss.netty.channel.Channels._
import org.jboss.netty.channel.{ChannelPipeline, SimpleChannelUpstreamHandler, ChannelPipelineFactory}
import ql2.Ql2.Response

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/16/14
 * Time: 11:52 AM
 *
 */


trait RethinkPipelineFactory extends ChannelPipelineFactory{

  // stateless
  val defaultHandler:SimpleChannelUpstreamHandler

  val frameDecoder:RethinkFrameDecoder
  val encoder = new RethinkDBEncoder

  def getPipeline: ChannelPipeline = {
    val p = pipeline()


    p.addLast("frameDecoder", frameDecoder)
    p.addLast("encoder", encoder)
    p.addLast("handler", defaultHandler)
    p
  }
}

object JsonPipelineFactory extends RethinkPipelineFactory{

  override val defaultHandler: SimpleChannelUpstreamHandler = new JsonChannelHandler
  override val frameDecoder: RethinkFrameDecoder =new JsonFrameDecoder()
}
object ProtoPipelineFactory extends RethinkPipelineFactory{

  override val defaultHandler: SimpleChannelUpstreamHandler = new ProtoChannelHandler
  override val frameDecoder: RethinkFrameDecoder = new ProtoFrameDecoder()

  val protoDecoder = ProtobufDecoder(Response.getDefaultInstance)
  override def getPipeline = {
    val p = super.getPipeline
    p.addAfter("frameDecoder","protoDecoder",protoDecoder)
    p
  }
}

