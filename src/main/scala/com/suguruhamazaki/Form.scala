package com.suguruhamazaki

sealed trait Form

trait Atom extends Form

trait Number extends Atom

case class Integer(value: Int) extends Number

case class Double(value: scala.Double) extends Number

case class Forms(forms: List[Form]) extends Form

case class Symbol(value: String) extends Atom

object Nil extends Symbol("nil")
