package com.suguruhamazaki

import scala.io.Source
import scala.util.Try

object TinyLisp extends Parsers with FormEvaluator {
  def globalEnvironment: Map[Symbol, Form] =
    Map(
      Symbol("+") → Plus,
      Symbol("-") → Minus,
      Symbol("*") → Multiply,
      Symbol("/") → Divide,
      Symbol("<") → LowerThan,
      Symbol("=") → Equals,
      Symbol("and") → And,
      Symbol("or") → Or,
      Symbol("not") → Not,
      Symbol("lambda") → Lambda,
      Symbol("if") → If
    )
  def main(args: Array[String]): Unit = {
    val input = Source.stdin.mkString
    val evaluated = parseAndEval(input)
    val result = evaluated.recover { case ex ⇒ ex.getMessage }.get
    println(result)
  }
  def parse(input: String): Try[Form] =
    parseAll(form, input) match {
      case Success(result, _) ⇒ scala.util.Success(result)
      case error: NoSuccess ⇒ scala.util.Failure(new RuntimeException(error.msg))
    }
  def eval(form: Form): Try[Form] = form match {
    case a: Atom ⇒ Evaluator.eval(a, globalEnvironment)
    case n: Forms ⇒ Evaluator.eval(n, globalEnvironment)
  }
  def parseAndEval(input: String): Try[String] =
    parse(input).flatMap(eval _).map(_.toString)

}
