package com.rethinkscala

import com.rethinkscala.ast._
import com.rethinkscala.ast.Var
import scala.reflect.macros.Context

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 7/4/13
 * Time: 3:00 PM 
 */
package object lifted {

  trait LiftedTyped[T]

  class Helper[C <: Context](val context: C) {

    import context.universe._

    class FunctionContext(args: List[(String, ValDef)]) {

      def method(qualifier: Tree, name: String, args: List[Tree]) = Apply(Select(qualifier, newTermName(name)), args)

      def apply(arg: String, name: String) = args.filter(_._1 != arg).headOption.map {
        v => method(v._2.rhs, "field", List(reify(name).tree))
      }
    }


    def generate(tree: Tree, ctx: Option[FunctionContext] = None): Expr[_] = tree match {
      case Function(vparms, body) => {
        val ctx = new FunctionContext(extract(vparms))

        generate(body, Some(ctx))
      }
      case Apply(fun, args) =>
    }


    def extract(vparms: List[ValDef]) = vparms.map {
      v => {
        val name = v.name.decoded
        val valDef = ValDef(Modifiers(v.mods.flags), newTermName(name), TypeTree(typeOf[Var]), EmptyTree)
        (name, valDef)
      }
    }


  }

  implicit def toLiftedPredicate1[T](f: T => Typed) = macro Impl.predicate1[T, Typed]

  private object Impl {


    def predicate1[T: c.WeakTypeTag, R: c.WeakTypeTag](c: Context)(f: c.Expr[T => R]): c.Expr[Predicate1] = {
      val helper = new Helper[c.type](c)
      helper.generate(f.tree)
      c.universe.reify {
        new Predicate1((v: Var) => v)
      }

    }
  }


}
