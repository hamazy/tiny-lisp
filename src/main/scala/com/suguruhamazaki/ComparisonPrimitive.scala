package com.suguruhamazaki

import scala.util.{ Failure, Success, Try }
import scala.collection.immutable.{ Nil ⇒ Empty }

trait BinaryNumericFunction[T] extends Function {
  def op(first: Int, second: Int): T
  def op(first: Int, second: scala.Double): T
  def op(first: scala.Double, second: Int): T
  def op(first: scala.Double, second: scala.Double): T
}

trait BinaryNumericComparisonFunction extends BinaryNumericFunction[Boolean] {

  def apply(env: Map[Symbol, Form], args: Form*): Try[Form] = args match {
    case Integer(first) :: Integer(second) :: Empty if op(first, second) ⇒ Success(True)
    case Double(first) :: Double(second) :: Empty if op(first, second) ⇒ Success(True)
    case Integer(first) :: Double(second) :: Empty if op(first, second) ⇒ Success(True)
    case Double(first) :: Integer(second) :: Empty if op(first, second) ⇒ Success(True)
    case first :: second :: Empty ⇒ Success(Nil)
    case other ⇒ Failure(new RuntimeException(s"Wrong number of arguments: $other"))
  }
}

object LowerThan extends BinaryNumericComparisonFunction {
  def op(first: Int, second: Int): Boolean = first < second
  def op(first: Int, second: scala.Double): Boolean = first < second
  def op(first: scala.Double, second: Int): Boolean = first < second
  def op(first: scala.Double, second: scala.Double): Boolean = first < second
}

object Equals extends BinaryNumericComparisonFunction {
  def op(first: Int, second: Int): Boolean = first == second
  def op(first: Int, second: scala.Double): Boolean = first == second
  def op(first: scala.Double, second: Int): Boolean = first == second
  def op(first: scala.Double, second: scala.Double): Boolean = first == second
}
