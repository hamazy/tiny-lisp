package com.suguruhamazaki

import scala.collection.immutable.{ Nil => Empty }
import scala.util.{ Failure, Success, Try }

trait BinaryNumericFoldableFunction extends BinaryNumericFunction[Number] {
  def apply(env: Map[Symbol, Form], args: Form*): Try[Form] = args match {
    case (last: Number) :: Empty ⇒ Success(last)
    case Integer(head) :: rest ⇒
      apply(env, rest: _*).flatMap {
        _ match {
          case Integer(sum) ⇒ Success(op(head, sum))
          case Double(sum) ⇒ Success(op(head, sum))
          case other ⇒ failure(other)
        }
      }
    case Double(head) :: rest ⇒
      apply(env, rest: _*).flatMap {
        _ match {
          case Integer(sum) ⇒ Success(op(head, sum))
          case Double(sum) ⇒ Success(op(head, sum))
          case other ⇒ failure(other)
        }
      }
    case other ⇒ failure(other: _*)
  }
  protected def failure(forms: Form*): Try[Form]
}

object Plus extends BinaryNumericFoldableFunction {
  def op(first: Int, second: Int): Number = Integer(first + second)
  def op(first: Int, second: scala.Double): Number = Double(first + second)
  def op(first: scala.Double, second: Int): Number = Double(first + second)
  def op(first: scala.Double, second: scala.Double): Number = Double(first + second)
  protected def failure(forms: Form*): Try[Form] =
    Failure(new RuntimeException(s"Can't apply plus operation on $forms"))
}

object Multiply extends BinaryNumericFoldableFunction {
  def op(first: Int, second: Int): Number = Integer(first * second)
  def op(first: Int, second: scala.Double): Number = Double(first * second)
  def op(first: scala.Double, second: Int): Number = Double(first * second)
  def op(first: scala.Double, second: scala.Double): Number = Double(first * second)
  protected def failure(forms: Form*): Try[Form] =
    Failure(new RuntimeException(s"Can't apply multiply operation on $forms"))
}
