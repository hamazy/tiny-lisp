package com.suguruhamazaki

import scala.collection.immutable.{ Nil => Empty }
import scala.util.{ Failure, Success, Try }

object NilOnlyIfNil extends ((Form) ⇒ Try[Form]) {
  def apply(form: Form): Try[Form] = form match {
    case Nil ⇒ Success(Nil)
    case other ⇒ Success(other)
  }
}

object And extends SpecialFormOperator {
  def apply(env: Map[Symbol, Form], args: Form*): Try[Form] = args match {
    case Nil :: Empty ⇒ Success(Nil)
    case (last: Atom) :: Empty ⇒ Evaluator.eval(last, env).flatMap(NilOnlyIfNil)
    case (last: Forms) :: Empty ⇒ Evaluator.eval(last, env).flatMap(NilOnlyIfNil)
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

object Or extends SpecialFormOperator {
  def apply(env: Map[Symbol, Form], args: Form*): Try[Form] = args match {
    case Nil :: Empty ⇒ Success(Nil)
    case (last: Atom) :: Empty ⇒ Evaluator.eval(last, env).flatMap(NilOnlyIfNil)
    case (last: Forms) :: Empty ⇒ Evaluator.eval(last, env).flatMap(NilOnlyIfNil)
    case (first: Atom) :: rest ⇒ Evaluator.eval(first, env).flatMap {
      case Nil ⇒ apply(env, rest: _*)
      case other ⇒ Success(other)
    }
    case (first: Forms) :: rest ⇒ Evaluator.eval(first, env).flatMap {
      case Nil ⇒ apply(env, rest: _*)
      case other ⇒ Success(other)
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
