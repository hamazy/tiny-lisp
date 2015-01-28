package com.suguruhamazaki

import scala.util.{ Failure, Success, Try }

object Lambda extends SpecialFormOperator {
  def apply(envAtDefinition: Map[Symbol, Form], args: Form*): Try[Form] = args match {
    case Forms(formalParams) :: body ⇒ Success(new Function {
      def apply(envAtRuntime: Map[Symbol, Form], argsAtRutime: Form*): Try[Form] = {
        if (formalParams.length != argsAtRutime.length) Failure(new RuntimeException("Wrong number of argument."))
        else body.foldLeft(Success(Nil): Try[Form]) { (result, partOfBody) ⇒
          val mergedEnv = (envAtDefinition ++ envAtRuntime) ++ Map(formalParams.zip(argsAtRutime).map { case (k: Symbol, v) ⇒ k → v }: _*)
          partOfBody match {
            case a: Atom ⇒ Evaluator.eval(a, mergedEnv)
            case f: Forms ⇒ Evaluator.eval(f, mergedEnv)
          }
        }
      }
    })
    // Expects no arguments. Thus, formal paramerters are evaluated as Nil
    case Nil :: body ⇒ Success(new Function {
      def apply(envAtRuntime: Map[Symbol, Form], argsAtRutime: Form*): Try[Form] = {
        if (argsAtRutime.length > 0) Failure(new RuntimeException("Wrong number of argument."))
        else body.foldLeft(Success(Nil): Try[Form]) { (result, partOfBody) ⇒
          val mergedEnv = (envAtDefinition ++ envAtRuntime)
          partOfBody match {
            case a: Atom ⇒ Evaluator.eval(a, mergedEnv)
            case f: Forms ⇒ Evaluator.eval(f, mergedEnv)
          }
        }
      }
    })
    case other ⇒ Failure(new RuntimeException(s"Unexpected syntax: $other"))
  }
}
