package com.rethinkscala.net


import com.rethinkscala.reflect.Reflector
import com.rethinkscala.{GeneratesKeys, Term, Document}
import com.rethinkscala.ast.{After, WithLifecycle}

object Translate {
  def translate[In, Out](implicit ct: Manifest[Out]): Translate[In, Out] = new BaseTranslate[In, Out] {}


}

trait Translate[In, Out] {
  lazy val fromMap = new MapConversion[Out] {
    protected def _convert(value: Map[String, Any], json: String)(implicit ct: Manifest[Out]) = {
      val out = Reflector.fromJson[Out](json)
      val doc = out.asInstanceOf[Document]
      doc._underlying = value
      doc._raw = json
      out
    }
  }

  def write(value: Out): Any

  def read(value: In, json: String, term: Term)(implicit ct: Manifest[Out]): Out

}

trait WithConversion[In, Out] {

  def convert(value: In, json: String, term: Term)(implicit ct: Manifest[Out]): Out = {

    val rtn = _convert(value, json)

    if (rtn.isInstanceOf[GeneratesKeys]) {
      val gks = rtn.asInstanceOf[GeneratesKeys]

      term match {
        case wlf: WithLifecycle[_] => wlf(After(gks.generatedKeys))

        case _ =>
      }

    }
    rtn

  }

  protected def _convert(value: In, json: String)(implicit ct: Manifest[Out]): Out
}

trait WithIterableConversion[Out] extends WithConversion[Iterable[Map[String, _]], Iterable[Out]] {
  protected def _convert(value: Iterable[Map[String, _]], json: String)(implicit ct: Manifest[Out]) = {
    val isDocument = classOf[Document] isAssignableFrom ct.runtimeClass
    val out = Reflector.fromJson[Iterable[Out]](json)

    if (isDocument) {
      val seq = value.toSeq
      out.zipWithIndex foreach {
        case (o: Document, i) => o._underlying = seq(i)
      }
    }
    out
  }
}

trait MapConversion[Out] extends WithConversion[Map[String, Any], Out]

trait BinaryConversion extends MapConversion[Boolean] {
  val resultField: String

  protected def _convert(value: Map[String, Any], json: String)(implicit mf: Manifest[Boolean]): Boolean = value.get(resultField).getOrElse(0) == 1
}

trait DocumentConversion[Out <: Document] extends MapConversion[Out] {

  protected def _convert(value: Map[String, Any], json: String)(implicit ct: Manifest[Out]): Out = {
    val out = Reflector.fromJson[Out](json)
    out._underlying = value
    out._raw = json
    out
  }

}

trait BaseTranslate[In, Out] extends Translate[In, Out] {

  type Conversion = WithConversion[In, Out]

  def write(value: Out): Any = value

  def read(value: In, json: String, term: Term)(implicit ct: Manifest[Out]): Out = {

    val isDocument = classOf[Document] isAssignableFrom ct.runtimeClass
    term match {
      case c: Conversion => c.convert(value, json, term)

      case _ => if (isDocument) fromMap.convert(value.asInstanceOf[Map[String, Any]], json, term) else value.asInstanceOf[Out]

    }

  }
}

