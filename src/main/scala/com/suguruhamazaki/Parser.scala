package com.suguruhamazaki

import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

trait Parsers extends JavaTokenParsers {

  def form: Parser[Form] = atom | "(" ~> forms <~ ")" | "(" ~ ")" ^^ (_ ⇒ Nil)
  def atom: Parser[Atom] = number | boolean | symbol
  def forms: Parser[Form] = rep1(form) ^^ (fs ⇒ Forms(fs))
  def number: Parser[Number] = "-".? ~ decimalNumber ^^ { s ⇒
    val n = s._1.map(_ + s._2).getOrElse(s._2)
    Try(Integer(n.toInt)).getOrElse(Double(n.toDouble))
  }
  def boolean: Parser[Atom] = "nil" ^^ (_ ⇒ Nil) | "t" ^^ (_ ⇒ True)
  // any visible characters except for `(` and `)`.
  def symbol: Parser[Symbol] = """([\p{Graph}&&[^()]])+""".r ^^ {
    case s ⇒ Symbol(s)
  }
}
