package com.suguruhamazaki

import org.scalatest.{ FlatSpec, GivenWhenThen }
import org.scalatest.Matchers

class ParserTest extends FlatSpec with GivenWhenThen with Matchers with Parsers {

  "Parser" should "parse an integer number" in {

    Given("an integer number")
    val input = "123"

    When("parse it")
    val actual = parseAll(form, input)

    Then("the corresponding Number is returned")
    actual.get should be(Integer(123))
  }

  it should "parse a double number" in {

    Given("an double number")
    val input = "12.3"

    When("parse it")
    val actual = parseAll(form, input)

    Then("the corresponding Double is returned")
    actual.get should be(Double(12.3))
  }

  it should "parse a symbol" in {

    Given("a symbol")
    val input = "foo"

    When("parse it")
    val actual = parseAll(form, input)

    Then("the corresponding Symbol is returned")
    actual.get should be(Symbol("foo"))
  }

  it should "parse nil" in {

    Given("a symbol, nil")
    val input = "nil"

    When("parse it")
    val actual = parseAll(form, input)

    Then("Nil is returned")
    actual.get should be(Nil)
  }

  it should "parse an empty list" in {

    Given("an empty list")
    val input = "()"

    When("parse it")
    val actual = parseAll(form, input)

    Then("Nil is returned")
    actual.get should be(Nil)
  }

  it should "parse a list" in {

    Given("a list")
    val input = "(foo 123 nil)"

    When("parse it")
    val actual = parseAll(form, input)

    Then("The corresponding Form is returned")
    actual.get should be(Forms(List(Symbol("foo"), Integer(123), Nil)))
  }

  it should "parse a nested list" in {

    Given("a nested list")
    val input = "(foo 123 (bar (buz)))"

    When("parse it")
    val actual = parseAll(form, input)

    Then("The corresponding Form is returned")
    actual.get should be(
      Forms(List(Symbol("foo"), Integer(123),
        Forms(List(Symbol("bar"),
          Forms(List(Symbol("buz"))))))))
  }

}
