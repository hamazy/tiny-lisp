package com.suguruhamazaki

import scala.util.{ Failure, Success, Try }
import scala.collection.immutable.{ Nil ⇒ Empty }

object If extends SpecialFormOperator {
  def apply(env: Map[Symbol, Form], args: Form*): Try[Form] = args match {
    case (test: Form) :: (trueCase: Form) :: (falseCase: Form) :: Empty ⇒
      val foo = test match {
        case a: Atom ⇒ Evaluator.eval(a, env)
        case fx: Forms ⇒ Evaluator.eval(fx, env)
      }
      val form = foo.map {
        case Nil => falseCase
        case _ ⇒ trueCase
      }
      form.flatMap {
        case a: Atom ⇒ Evaluator.eval(a, env)
        case fx: Forms ⇒ Evaluator.eval(fx, env)
      }
    case other ⇒ Failure(new RuntimeException(s"Wrong number of arguments: $other"))
  }
}
