package com.suguruhamazaki

import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

trait Parsers extends JavaTokenParsers {

  def form: Parser[Form] = atom | "(" ~> forms <~ ")" | "(" ~ ")" ^^ (_ ⇒ Nil)
  def atom: Parser[Atom] = number | symbol
  def forms: Parser[Form] = rep1(form) ^^ (fs ⇒ Forms(fs))
  def number: Parser[Number] = decimalNumber ^^ { s ⇒
    Try(Integer(s.toInt)).getOrElse(Double(s.toDouble))
  }
  def symbol: Parser[Symbol] = ident ^^ {
    case "nil" ⇒ Nil
    case s ⇒ Symbol(s)
  }
}
