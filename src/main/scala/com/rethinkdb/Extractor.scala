package com.rethinkdb

import scala.reflect.ClassTag

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 6/14/13
 * Time: 3:23 PM
 * To change this template use File | Settings | File Templates.
 */
trait Extractor[T]{

  implicit def tag:ClassTag[T]


}
