package com.rethinkscala.ast

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 1/17/14
 * Time: 11:58 AM 
 */
abstract class JoinPredicate extends Function2[Var, Var, Binary]

abstract class MappingFunction[T] extends Function1[Var, T]

abstract class ReductionFunction[T] extends Function1[Var, Stream[T]]