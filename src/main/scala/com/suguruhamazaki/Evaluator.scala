package com.suguruhamazaki

import scala.util.{ Failure, Success, Try }

trait Evaluator[T] {
  def eval(form: T, env: Map[Symbol, Form]): Try[Form]
}

object Evaluator extends FormEvaluator {
  def eval[T](form: T, env: Map[Symbol, Form])(implicit ev: Evaluator[T]): Try[Form] =
    ev.eval(form, env)
}

trait FormEvaluator extends AtomEvaluator with FormsEvaluator

trait AtomEvaluator extends NumberEvaluator with SymbolEvaluator {
  implicit val atomEvaluator = new Evaluator[Atom] {
    def eval(atom: Atom, env: Map[Symbol, Form]): Try[Form] = atom match {
      case s: Symbol ⇒ symbolEvaluator.eval(s, env)
      case other ⇒ evalToItself(other)
    }
  }
}

trait NumberEvaluator extends SelfReturningEvaluator {
  implicit val integerEvaluator = new Evaluator[Integer] {
    def eval(i: Integer, env: Map[Symbol, Form]): Try[Form] = evalToItself(i)
  }
  implicit val doubleEvaluator = new Evaluator[Double] {
    def eval(d: Double, env: Map[Symbol, Form]): Try[Form] = evalToItself(d)
  }
}

trait SelfReturningEvaluator {
  def evalToItself(atom: Atom): Try[Form] = Success(atom)
}

trait SymbolEvaluator {
  implicit val symbolEvaluator = new Evaluator[Symbol] {
    def eval(s: Symbol, env: Map[Symbol, Form]): Try[Form] =
      env.get(s).map(Success(_)).getOrElse(Failure(new RuntimeException(s"Unbound symbol ${s}")))
  }
}

trait FormsEvaluator extends AtomEvaluator {
  import scala.collection.immutable.Nil
  implicit val formsEvaluator = new Evaluator[Forms] {
    // evaluate a function with no argument
    private def evalNoArgFunction(env: Map[Symbol, Form]): Form ⇒ Try[Form] = {
      case f: Function ⇒ eval(Forms(List(f)), env)
      case other ⇒ Failure(new RuntimeException(s"${other} is not a function."))
    }
    def eval(forms: Forms, env: Map[Symbol, Form]): Try[Form] = forms.forms match {
      case (op: Symbol) :: Nil ⇒
        Evaluator.eval(op, env) flatMap (evalNoArgFunction(env))
      case (op: Function) :: Nil ⇒ op.apply(env)
      case (forms: Forms) :: Nil ⇒ eval(forms, env).flatMap(evalNoArgFunction(env))
      case (op: Symbol) :: rest ⇒
        Evaluator.eval(op, env).flatMap {
          case opEvaluated ⇒ eval(Forms(opEvaluated :: rest), env)
        }
      case (op: Function) :: rest ⇒
        // evaluate each element of rest.
        val tries = rest.map {
          case atom: Atom ⇒ Evaluator.eval(atom, env)
          case forms: Forms ⇒ eval(forms, env)
        }
        // convert Tries of Form into Try of Forms
        val tryOfArgs = tries.foldRight(Success(List()): Try[List[Form]]) { (tryForm, result) ⇒
          for {
            form ← tryForm
            list ← result
          } yield (form :: list)
        }
        // then, apply them to op.
        tryOfArgs.flatMap { args ⇒
          op.apply(env, args: _*)
        }
      case (forms: Forms) :: rest ⇒ eval(forms, env).flatMap {
        case opEvaluated ⇒ eval(Forms(opEvaluated :: rest), env)
      }
      case (op: SpecialFormOperator) :: rest ⇒
        op.apply(env, rest: _*)
      case other ⇒ Failure(new RuntimeException(s"Can't evaluate $other"))
    }
  }
}
