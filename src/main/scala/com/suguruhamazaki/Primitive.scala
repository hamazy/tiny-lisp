package com.suguruhamazaki

import scala.util.{ Failure, Success, Try }

object Plus extends Function {
  import scala.collection.immutable.Nil
  def apply(env: Map[Symbol, Form], args: Form*): Try[Form] = args match {
    case (last: Integer) :: Nil ⇒ Success(last)
    case (last: Double) :: Nil ⇒ Success(last)
    case Integer(head) :: rest ⇒
      apply(env, rest: _*).flatMap {
        _ match {
          case Integer(sum) ⇒ Success(Integer(head + sum))
          case Double(sum) ⇒ Success(Double(head + sum))
          case other ⇒ Failure(new RuntimeException(s"Can't apply plus operation on $other"))
        }
      }
    case Double(head) :: rest ⇒
      apply(env, rest: _*).flatMap {
        _ match {
          case Integer(sum) ⇒ Success(Double(head + sum))
          case Double(sum) ⇒ Success(Double(head + sum))
          case other ⇒ Failure(new RuntimeException(s"Can't apply plus operation on $other"))
        }
      }
    case other ⇒ Failure(new RuntimeException(s"Can't apply plus operation on $other"))
  }
}
