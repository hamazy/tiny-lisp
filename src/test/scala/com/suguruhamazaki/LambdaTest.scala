package com.suguruhamazaki

import org.scalatest.{ FlatSpec, GivenWhenThen, Matchers }
import scala.util.Failure
import scala.util.Success

class LambdaTest extends FlatSpec with GivenWhenThen with Matchers {

  "Evaluator" should "evaluate lambda as function" in {

    val formalParams = Forms(List(Symbol("x")))
    val body = Forms(List(Plus, Symbol("x"), Symbol("x")))
    val lambda = Forms(List(Lambda, formalParams, body))

    val actual = Evaluator.eval(lambda, Map[Symbol, Form]())

    actual shouldBe a[Success[_]]
    actual.get shouldBe a[Function]
  }

  it should "evaluate lambda application with no argument" in {

    val formalParams = Nil
    val body = Integer(123)
    val lambda = Forms(List(Lambda, formalParams, body))

    val input = Forms(List(lambda))

    val actual = Evaluator.eval(input, Map[Symbol, Form]())

    actual should be(Success(Integer(123)))
  }

  it should "evaluate lambda application with body containing multiple expressions" in {

    val formalParams = Nil
    val lambda = Forms(List(Lambda, formalParams, Integer(123), Integer(456)))

    val input = Forms(List(lambda))

    val actual = Evaluator.eval(input, Map[Symbol, Form]())

    actual should be(Success(Integer(456)))
  }

  it should "evaluate lambda application with one argument" in {

    val formalParams = Forms(List(Symbol("x")))
    val body = Forms(List(Plus, Symbol("x"), Symbol("x")))
    val lambda = Forms(List(Lambda, formalParams, body))

    val input = Forms(List(lambda, Integer(1)))

    val actual = Evaluator.eval(input, Map[Symbol, Form]())

    actual should be(Success(Integer(2)))
  }

  it should "evaluate lambda application with more arguments" in {

    val formalParams = Forms(List(Symbol("x"), Symbol("y")))
    val body = Forms(List(Plus, Symbol("x"), Symbol("y")))
    val lambda = Forms(List(Lambda, formalParams, body))

    val input = Forms(List(lambda, Integer(1), Integer(2)))

    val actual = Evaluator.eval(input, Map[Symbol, Form]())

    actual should be(Success(Integer(3)))
  }

  it should "evaluate lambda application that captures variable in lexical scope" in {

    val formalParams = Forms(List(Symbol("x"), Symbol("y")))
    val body = Forms(List(Plus, Symbol("x"), Symbol("y"), Symbol("z")))
    val lambda = Forms(List(Lambda, formalParams, body))

    val input = Forms(List(lambda, Integer(1), Integer(2)))

    val actual = Evaluator.eval(input, Map[Symbol, Form](Symbol("z") → Integer(3)))

    actual should be(Success(Integer(6)))
  }

  it should "evaluate lambda application with no formal parameter and unexpected extra argument" in {

    val formalParams = Nil
    val body = Integer(123)
    val lambda = Forms(List(Lambda, formalParams, body))

    val input = Forms(List(lambda, Integer(456))) // unexpected extra argument

    val actual = Evaluator.eval(input, Map[Symbol, Form]())

    actual shouldBe a[Failure[_]]
  }

  it should "evaluate lambda application with some formal parameters and unmatched number of arguments" in {

    val formalParams = Forms(List(Symbol("x")))
    val body = Forms(List(Plus, Symbol("x"), Symbol("x")))
    val lambda = Forms(List(Lambda, formalParams, body))

    val input = Forms(List(lambda, Integer(1), Integer(2)))

    val actual = Evaluator.eval(input, Map[Symbol, Form]())

    actual shouldBe a[Failure[_]]
  }
}
