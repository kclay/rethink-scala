package com.rethinkdb.utils

import scala.reflect.ClassTag
import com.thoughtworks.paranamer.{CachingParanamer, BytecodeReadingParanamer}

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 6/15/13
 * Time: 10:37 PM
 * To change this template use File | Settings | File Templates.
 */
object Pickle {
  import scala.reflect.runtime.{universe=>ru}
  import ru._
  import com.rethinkdb.Field

  import reflect.runtime.currentMirror
  val fieldAnnotationType=ru.typeOf[Field]



  val pn = new CachingParanamer(new BytecodeReadingParanamer)
  def wrap[T](value:Map[String,Any])(implicit tt:TypeTag[T])={


    val asClass = tt.tpe.typeSymbol.asClass
    val ctor = tt.tpe.declaration(ru.nme.CONSTRUCTOR).asMethod
    val mapping = ctor.paramss.head.map{ p=>

      val mapTo = p.annotations.find(_.tpe == fieldAnnotationType).map{
        f=>f.scalaArgs.head.productElement(0).asInstanceOf[ru.Constant].value.asInstanceOf[String]
      }
      (p.name.decoded,mapTo)
    }

    val args = mapping.collect{
      case (native:String,Some(json:String))=>json
      case (native:String,None)=>native
    }.map(value.get(_).get)

    val cm =currentMirror.reflectClass(asClass)
    val ctorm = cm.reflectConstructor(ctor)

     ctorm.apply(args: _*).asInstanceOf[T]




  }
}
