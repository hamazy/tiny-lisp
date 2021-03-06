package com.suguruhamazaki

import scala.util.Try

sealed trait Form

trait Atom extends Form

trait Number extends Atom

case class Integer(value: Int) extends Number {
  override def toString(): String = value.toString
}

case class Double(value: scala.Double) extends Number {
  override def toString(): String = value.toString
}

case class Forms(forms: List[Form]) extends Form

case class Symbol(value: String) extends Atom {
  override def toString(): String = value
}

object Nil extends Atom {
  override def toString(): String = "nil"
}

object True extends Atom {
  override def toString(): String = "t"
}

abstract class Function extends Atom {
  /**
   * @param args Forms which are already evaluated
   * @return evaluatend Form
   */
  def apply(env: Map[Symbol, Form], args: Form*): Try[Form]
}

abstract class SpecialFormOperator extends Atom {
  /**
   * @param args Forms which are NOT evaluated yet.
   * @return evaluatend Form
   */
  def apply(env: Map[Symbol, Form], args: Form*): Try[Form]
}
