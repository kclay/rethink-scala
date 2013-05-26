package com.rethinkdb

import com.rethinkdb.ast._
import com.rethinkdb.ast.StringDatum
import com.rethinkdb.ast.BooleanDatum
import com.rethinkdb.ast.NumberDatum
import scala.Some


/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 5/25/13
 * Time: 5:30 PM
 * To change this template use File | Settings | File Templates.
 */
object ConvertFrom {

  import ql2.{Ql2 => p, Backtrace, Response, Frame => QFrame}

  import com.rethinkdb._


  import com.rethinkdb.RethinkCompileError
  import com.rethinkdb.RethinkClientError
  import com.rethinkdb.Frame

  import com.rethinkdb.RethinkRuntimeError
  import scala.Some

  import Response.ResponseType._


  implicit def backtrace2Frames(backtrace: Option[Backtrace]): Iterable[Frame] = {
    backtrace.map {
      b => b.`frames`.map {
        f => Frame(Some(f match {
          case QFrame.FrameType.POS => PositionFrame
          case QFrame.FrameType.OPT => OptionalFrame
        }), f.`pos`, f.`opt`)
      }
    }.getOrElse(Seq.empty[Frame])

  }

  implicit def datumToString(d: ql2.Datum): String = d.`rStr`.getOrElse("")


  //implicit def datnumCollection2Iterable(d:JList[p.Datum]):Iterable[]


  def toError(response: Response, term: Term): RethinkError = {

    val message: String = response.`response`(0)
    val frames: Iterable[Frame] = response.`backtrace`

    response.`type` match {
      case RUNTIME_ERROR => RethinkRuntimeError(message, term, frames)
      case COMPILE_ERROR => RethinkCompileError(message, term, frames)
      case CLIENT_ERROR => RethinkClientError(message, term, frames)
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
  implicit def boolToDataNum(b: Boolean): Datum = BooleanDatum(b)

  implicit def intToDatNum(i: Int): Datum = NumberDatum(i)

  implicit def longToDatNum(l: Long): Datum = NumberDatum(l)

  implicit def floatToDatNum(f: Float): Datum = NumberDatum(f)

  implicit def string2DatNum(s: String): Datum = StringDatum(s)

  implicit def bool2Option(value: Boolean): Option[Boolean] = Some(value)

  implicit def string2Option(value: String): Option[String] = Some(value)

  implicit def int2Option(value: Int): Option[Int] = Some(value)

  implicit def string2DB(name: String): DB = DB(name)


}
