package com.suguruhamazaki

import org.scalatest.{ FlatSpec, GivenWhenThen, Matchers }
import scala.util.{ Failure, Success }

class EvaluatorTest extends FlatSpec with GivenWhenThen with Matchers {

  "Evaluator" should "evaluate an integer" in {

    Given("an Integer")
    val input = Integer(123)

    When("evaluate it")
    val actual = Evaluator.eval(input, Map[Symbol, Form]())

    Then("the Integer itself is returned")
    actual.get should be(input)
  }

  it should "evaluate a double" in {

    Given("a Double")
    val input = Double(12.3)

    When("evaluate it")
    val actual = Evaluator.eval(input, Map[Symbol, Form]())

    Then("the Double itself is returned")
    actual.get should be(input)
  }

  it should "evaluate an unbounded symbol" in {

    Given("a unbounded symbol")
    val input = Symbol("foo")

    When("evaluate it")
    val actual = Evaluator.eval(input, Map[Symbol, Form]())

    Then("a failure is returned")
    actual shouldBe a[Failure[_]]
  }

  it should "evaluate a bounded symbol" in {

    Given("a unbounded symbol")
    val input = Symbol("foo")

    When("evaluate it")
    val actual = Evaluator.eval(input, Map[Symbol, Form](Symbol("foo") → Integer(123)))

    Then("the bound value is returned")
    actual should be(Success(Integer(123)))
  }

  it should "evaluate a nil" in {

    Given("a symbol, nil")
    val input = Nil

    When("evaluate it")
    val actual = Evaluator.eval(input: Atom, Map[Symbol, Form]())

    Then("Nil itself is returned")
    actual shouldBe (Success(Nil))
  }

  it should "evaluate an addition with one operand" in {

    Given("an addition calculation")
    val input = Forms(List(Plus, Integer(2)))

    When("evaluate it")
    val actual = Evaluator.eval(input, Map[Symbol, Form]())

    Then("the answer is returned")
    actual should be(Success(Integer(2)))
  }

  it should "evaluate an addition with multiple operands" in {

    Given("an addition calculation")
    val input = Forms(List(Plus, Integer(2), Integer(3)))

    When("evaluate it")
    val actual = Evaluator.eval(input, Map[Symbol, Form]())

    Then("the answer is returned")
    actual should be(Success(Integer(5)))
  }

  it should "evaluate a nested addition" in {

    Given("a nested addition calculation")
    val input = Forms(List(
      Plus,
      Forms(List(Plus, Integer(2), Integer(3))),
      Forms(List(Plus, Integer(4), Integer(5)))
    ))

    When("evaluate it")
    val actual = Evaluator.eval(input, Map[Symbol, Form]())

    Then("the answer is returned")
    actual should be(Success(Integer(14)))
  }
}
