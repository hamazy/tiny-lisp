package com.suguruhamazaki

import org.scalatest.{ FlatSpec, GivenWhenThen, Matchers }
import scala.util.Success

class ConditionalPrimitiveTest extends FlatSpec with GivenWhenThen with Matchers {

  val emptyEnvironment: Map[Symbol, Form] = Map()

  "Evaluator" should "evaluate if special form with successful condition" in {

    Given("a list with condition returning non-nil")
    val input = Forms(List(If, Symbol("condition"), Symbol("truecase"), Symbol("falsecase")))

    When("evaluate it")
    val actual = Evaluator.eval(
      input, Map(Symbol("condition") → Integer(123), Symbol("truecase") → Integer(456))
    )

    Then("evaluated truecase is returned")
    actual should be(Success(Integer(456)))
  }

  it should "evaluate if special form with unsuccessful condition" in {

    Given("a list with condition returning non-nil")
    val input = Forms(List(If, Symbol("condition"), Symbol("truecase"), Symbol("falsecase")))

    When("evaluate it")
    val actual = Evaluator.eval(
      input, Map(Symbol("condition") → Nil, Symbol("falsecase") → Integer(456))
    )

    Then("evaluated truecase is returned")
    actual should be(Success(Integer(456)))
  }
}
