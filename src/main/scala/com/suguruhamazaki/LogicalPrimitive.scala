package com.suguruhamazaki

import scala.collection.immutable.{ Nil => Empty }
import scala.util.{ Success, Try }

object And extends SpecialFormOperator {
  val nilOnlyIfNil: Form ⇒ Try[Form] = {
    case Nil ⇒ Success(Nil)
    case other ⇒ Success(other)
  }
  def apply(args: Form*): Try[Form] = args match {
    case Nil :: Empty ⇒ Success(Nil)
    case (last: Atom) :: Empty ⇒ Evaluator.eval(last).flatMap(nilOnlyIfNil)
    case (last: Forms) :: Empty ⇒ Evaluator.eval(last).flatMap(nilOnlyIfNil)
    case (first: Atom) :: rest ⇒ Evaluator.eval(first).flatMap {
      case Nil ⇒ Success(Nil)
      case other ⇒ apply(rest: _*)
    }
    case (first: Forms) :: rest ⇒ Evaluator.eval(first).flatMap {
      case Nil ⇒ Success(Nil)
      case other ⇒ apply(rest: _*)
    }
  }
}
