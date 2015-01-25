package com.suguruhamazaki

import scala.collection.immutable.{ Nil => Empty }
import scala.util.{ Failure, Success, Try }

object And extends SpecialFormOperator {
  val nilOnlyIfNil: Form ⇒ Try[Form] = {
    case Nil ⇒ Success(Nil)
    case other ⇒ Success(other)
  }
  def apply(env: Map[Symbol, Form], args: Form*): Try[Form] = args match {
    case Nil :: Empty ⇒ Success(Nil)
    case (last: Atom) :: Empty ⇒ Evaluator.eval(last, env).flatMap(nilOnlyIfNil)
    case (last: Forms) :: Empty ⇒ Evaluator.eval(last, env).flatMap(nilOnlyIfNil)
    case (first: Atom) :: rest ⇒ Evaluator.eval(first, env).flatMap {
      case Nil ⇒ Success(Nil)
      case other ⇒ apply(env, rest: _*)
    }
    case (first: Forms) :: rest ⇒ Evaluator.eval(first, env).flatMap {
      case Nil ⇒ Success(Nil)
      case other ⇒ apply(env, rest: _*)
    }
  }
}

object Not extends Function {
  def apply(env: Map[Symbol, Form], args: Form*): Try[Form] = args match {
    case Nil :: Empty ⇒ Success(True)
    case other :: Empty ⇒ Success(Nil)
    case other ⇒ Failure(new RuntimeException(s"Wrong number of arguments: ${other}"))
  }
}
