package com.rethinkscala

import com.rethinkscala.ast.{MethodAggregation, ProduceSequence, Typed, ProduceSeq}
import com.rethinkscala.changefeeds.ast.Changes

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 2/1/15
 * Time: 11:18 AM
 *
 */
package object changefeeds {



  final class ChangeFeedSupport[T](val target:Typed) extends AnyVal{
    def changes = new Changes[T](target)
  }



  trait ChangeFeedImplicits{


    implicit def seqToChangeFeed[T](seq:ProduceSequence[T]) =new ChangeFeedSupport(seq)
    implicit def aggToChangeFeed[T](aggregation:MethodAggregation[T]) = new ChangeFeedSupport(aggregation)
  }

}
