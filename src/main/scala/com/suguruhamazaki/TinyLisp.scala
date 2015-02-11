package com.suguruhamazaki

import scala.io.Source

object TinyLisp extends Parsers with FormEvaluator {
  def globalEnvironment: Map[Symbol, Form] =
    Map(
      Symbol("+") → Plus,
      Symbol("<") → LowerThan,
      Symbol("=") → Equals,
      Symbol("and") → And,
      Symbol("not") → Not,
      Symbol("lambda") → Lambda
    )
  def main(args: Array[String]): Unit = {
    val input = Source.stdin.mkString
    val parsed = parseAll(form, input).map {
      case a: Atom ⇒ Evaluator.eval(a, globalEnvironment)
      case n: Forms ⇒ Evaluator.eval(n, globalEnvironment)
    }
    val evaluated = parsed match {
      case Success(result, _) ⇒ result
      case error: NoSuccess ⇒ scala.util.Failure(new RuntimeException(error.msg))
    }
    val result = evaluated.map(_.toString).recover { case ex ⇒ ex.getMessage }.get
    println(result)
  }
}
