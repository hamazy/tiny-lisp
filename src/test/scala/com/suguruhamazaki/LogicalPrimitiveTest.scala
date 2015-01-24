package com.suguruhamazaki

import org.scalatest.{ FlatSpec, GivenWhenThen, Matchers }
import scala.util.Success
import scala.util.{ Failure, Success }
import com.suguruhamazaki.{ And ⇒ And_ }

class LogicalPrimitiveTest extends FlatSpec with GivenWhenThen with Matchers {

  "Evaluator" should "evaluate logical And of Nil" in {

    Given("a list with an And operator and a Nil")
    val input = Forms(List(And_, Nil))

    When("evaluate it")
    val actual = Evaluator.eval(input)

    Then("a Nil is returned")
    actual should be(Success(Nil))
  }

  it should "evaluate logical And of non-Nil" in {

    Given("a list with an And operator and a non-Nil")
    val input = Forms(List(And_, Integer(123)))

    When("evaluate it")
    val actual = Evaluator.eval(input)

    Then("a non-Nil is returned")
    actual shouldNot be(Success(Nil))
    actual should be(Success(Integer(123)))
  }

  it should "evaluate logical And of multiple Nils" in {

    Given("a list with an And operator and multiple atoms that do not contain a Nil ")
    val input = Forms(List(And_, Nil, Nil, Nil))

    When("evaluate it")
    val actual = Evaluator.eval(input)

    Then("a Nil is returned")
    actual should be(Success(Nil))
  }

  it should "evaluate logical And of atoms containing a non-Nil" in {

    Given("a list with an And operator and multiple atoms that contain a non-Nil ")
    val input = Forms(List(And_, Nil, Integer(123), Nil))

    When("evaluate it")
    val actual = Evaluator.eval(input)

    Then("a Nil is returned")
    actual should be(Success(Nil))
  }

  it should "evaluate logical And of atoms containing no Nil" in {

    Given("a list with an And operator and multiple atoms that do not contain a Nil ")
    val input = Forms(List(And_, Plus, Integer(123), Double(1.23)))

    When("evaluate it")
    val actual = Evaluator.eval(input)

    Then("a non-Nil is returned")
    actual shouldNot be(Success(Nil))
    actual should be(Success(Double(1.23)))
  }

  it should "evaluate logical And of atoms and forms containing no Nil" in {

    Given("a list with an And operator and multiple atoms that do not contain a Nil ")
    val input = Forms(List(And_, Plus, Integer(123), (Forms(List(And_, Integer(456))))))

    When("evaluate it")
    val actual = Evaluator.eval(input)

    Then("a non-Nil is returned")
    actual shouldNot be(Success(Nil))
    actual should be(Success(Integer(456)))
  }
}
