package com.rethinkdb.conversions

import ql2.{Ql2=>p}
import com.rethinkdb.response.{RethinkClientError, RethinkCompileError, RethinkRuntimeError, RethinkError}
import com.rethinkdb.Ast.Term

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 3/26/13
 * Time: 9:29 PM
 * To change this template use File | Settings | File Templates.
 */
object Java {
  import collection.JavaConverters._
  import com.rethinkdb.response.{Frame,PositionFrame,OptionalFrame}
  import p.Response.ResponseType._
  implicit def backtrace2Frames(backtrace:Option[p.Backtrace]):Iterable[Frame]={
    backtrace.map{
      b=> b.getFramesList.asScala.toList.map{
        f=> Frame(Some(f.getType match{
          case p.Frame.FrameType.POS => PositionFrame
          case p.Frame.FrameType.OPT =>OptionalFrame
        }),Some(f.getPos),Some(f.getOpt))
      }
    }.getOrElse(Seq.empty[Frame])

  }

  implicit def datumToString(d:p.Datum):String ={
    if(d.getType==p.Datum.DatumType.R_STR) d.getRStr else ""
  }


  def toError(response:p.Response,term:Term):RethinkError={
    import reflect.runtime.universe._
    import reflect.runtime.currentMirror
    val message:String = response.getResponse(0)
    val frames:Iterable[Frame] =Some(response.getBacktrace)

      response.getType match{
        case RUNTIME_ERROR=>RethinkRuntimeError(message,term,frames)
        case COMPILE_ERROR => RethinkCompileError(message,term,frames)
        case CLIENT_ERROR=>RethinkClientError(message,term,frames)
      }
   /*

    val typ=typeOf[T]
    val constructor=typ.declaration(nme.CONSTRUCTOR).asMethod
    val instance = currentMirror reflectClass typ.typeSymbol.asClass reflectConstructor constructor apply (message,term,frames)
    instance.asInstanceOf[T]
    */

  }
}
