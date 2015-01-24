package com.suguruhamazaki

import scala.util.{ Failure, Success, Try }

trait Evaluator[T] {
  def eval(form: T): Try[Form]
}

object Evaluator extends FormEvaluator {
  def eval[T](form: T)(implicit ev: Evaluator[T]): Try[Form] =
    ev.eval(form)
}

trait FormEvaluator extends AtomEvaluator with FormsEvaluator

trait AtomEvaluator extends NumberEvaluator with SymbolEvaluator {
  implicit val atomEvaluator = new Evaluator[Atom] {
    def eval(atom: Atom): Try[Form] = atom match {
      case i: Integer ⇒ integerEvaluator.eval(i)
      case d: Double ⇒ doubleEvaluator.eval(d)
    }
  }
}

trait NumberEvaluator {
  implicit val integerEvaluator = new Evaluator[Integer] {
    def eval(i: Integer): Try[Form] = Success(i)
  }
  implicit val doubleEvaluator = new Evaluator[Double] {
    def eval(d: Double): Try[Form] = Success(d)
  }
}

trait SymbolEvaluator {
  implicit val symbolEvaluator = new Evaluator[Symbol] {
    def eval(s: Symbol): Try[Form] = s match {
      case Nil ⇒ Success(Nil)
      case other ⇒ Failure(new RuntimeException(s"Unbound symbol ${other.value}"))
    }
  }
  // implicit val nilEvaluator: Evaluator[Nil] =  symbolEvaluator
}

trait FormsEvaluator extends AtomEvaluator {
  import scala.collection.immutable.Nil
  implicit val formsEvaluator = new Evaluator[Forms] {
    def eval(forms: Forms): Try[Form] = forms.forms match {
      case (op: Symbol) :: Nil ⇒
        Evaluator.eval(op) flatMap {
          case f: Function ⇒ eval(Forms(List(f)))
          case other ⇒ Failure(new RuntimeException(s"Can't apply $other"))
        }
      case (op: Function) :: Nil ⇒ op.apply()
      case (op: Symbol) :: rest ⇒
        Evaluator.eval(op).flatMap {
          case opEvaluated ⇒ eval(Forms(opEvaluated :: rest))
        }
      case (op: Function) :: rest ⇒
        // evaluate each element of rest.
        val tries = rest.map {
          case atom: Atom ⇒ Evaluator.eval(atom)
          case forms: Forms ⇒ eval(forms)
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
          op.apply(args: _*)
        }
      case other ⇒ Failure(new RuntimeException(s"Can't apply $other"))
    }
  }
}
