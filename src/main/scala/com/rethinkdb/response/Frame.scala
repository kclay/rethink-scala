package com.rethinkdb.response

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 3/26/13
 * Time: 9:36 PM
 * To change this template use File | Settings | File Templates.
 */

sealed trait FrameType
case object PositionFrame extends FrameType
case object OptionalFrame extends FrameType

case class Frame(frameType:Option[FrameType],pos:Option[Long],opt:Option[String]){

}
