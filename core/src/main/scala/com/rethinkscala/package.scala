package com

import com.rethinkscala.ast._


/** Created by IntelliJ IDEA.
  * User: Keyston
  * Date: 3/19/13
  * Time: 7:32 PM
  */
package object rethinkscala extends ImplicitConversions {

  private[rethinkscala] trait DatumOrFunction

  implicit val stringToStrings = new ToAst[String] {
    type TypeMember = Strings
  }
  implicit val doubleToNumeric = new ToAst[Double] {
    type TypeMember = Numeric
  }
  implicit val anyToTyped = new ToAst[Any] {
    type TypeMember = Typed
  }
  implicit val docToTyped = new ToAst[Document] {
    type TypeMember = Var
  }

}
