package com.rethinkscala

import com.rethinkscala.ast._
import com.rethinkscala.ast.StringDatum
import com.rethinkscala.ast.BooleanDatum
import com.rethinkscala.ast.NumberDatum
import scala.Some
import com.rethinkscala.net.{UnknownFrame, OptionalFrame, PositionFrame, RethinkError}
import scala.collection.JavaConversions._
import com.rethinkscala.ast.StringDatum
import com.rethinkscala.ast.BooleanDatum
import com.rethinkscala.ast.DB
import com.rethinkscala.ast.NumberDatum
import scala.Some

/** Created with IntelliJ IDEA.
  * User: keyston
  * Date: 5/25/13
  * Time: 5:30 PM
  * To change this template use File | Settings | File Templates.
  */
object ConvertFrom {

  import ql2.Ql2.{Backtrace, Response, Frame => QFrame}

  import scala.collection.JavaConversions._

  import scala.Some

  import Response.ResponseType._
  import com.rethinkscala.net.{RethinkCompileError, RethinkClientError, Frame, RethinkRuntimeError}


  implicit def backtrace2Frames(backtrace: Option[Backtrace]): Iterable[Frame] = {
    backtrace.map {
      b =>
        b.getFramesList.map {
          f =>
            Frame(Some(f match {
              case QFrame.FrameType.POS => PositionFrame
              case QFrame.FrameType.OPT => OptionalFrame
              case _ => UnknownFrame
            }), Option(f.getPos), Option(f.getOpt))
        }
    }.getOrElse(Seq.empty[Frame])

  }

  implicit def datumToString(d: ql2.Ql2.Datum): String = Option(d.getRStr).getOrElse("")

  //implicit def datnumCollection2Iterable(d:JList[p.Datum]):Iterable[]

  def toError(response: Response, term: Term): RethinkError = {

    val message: String = response.getResponse(0)
    val frames: Iterable[Frame] = Option(response.getBacktrace)

    response.getType match {
      case RUNTIME_ERROR => RethinkRuntimeError(message, term, frames)
      case COMPILE_ERROR => RethinkCompileError(message, term, frames)
      case CLIENT_ERROR => RethinkClientError(message, term, frames)
      case _ => RethinkRuntimeError(message, term, frames)
    }
  }

  /*

   val typ=typeOf[T]
   val constructor=typ.declaration(nme.CONSTRUCTOR).asMethod
   val instance = currentMirror reflectClass typ.typeSymbol.asClass reflectConstructor constructor apply (message,term,frames)
   instance.asInstanceOf[T]
   */

}

object ConvertTo {
  implicit def boolToDataNum(b: Boolean): BooleanDatum = BooleanDatum(b)

  implicit def intToDatNum(i: Int): NumberDatum = NumberDatum(i)

  implicit def longToDatNum(l: Long): NumberDatum = NumberDatum(l)

  implicit def floatToDatNum(f: Float): NumberDatum = NumberDatum(f)

  implicit def string2DatNum(s: String): StringDatum = StringDatum(s)

  implicit def bool2Option(value: Boolean): Option[Boolean] = Some(value)

  implicit def string2Option(value: String): Option[String] = Some(value)

  implicit def int2Option(value: Int): Option[Int] = Some(value)

  implicit def string2DB(name: String): DB = DB(name)

}
