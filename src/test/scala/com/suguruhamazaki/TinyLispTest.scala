package com.suguruhamazaki

import org.scalatest.{ FlatSpec, GivenWhenThen, Matchers }
import scala.util.Success

class TinyLispTest extends FlatSpec with GivenWhenThen with Matchers {

  "TinyLisp" should "comute Fibonacci number" in {
    val input =
      """|((lambda (or fib)
         |   (fib 20))
         | (lambda (a b)
         |   (if a t b))
         | (lambda (n)
         |   (if (or (< n 0) (= n 0))
         |       0
         |     (if (= n 1)
         |         1
         |       (+ (fib (- n 1))
         |          (fib (- n 2)))))))""".stripMargin

    val actual = TinyLisp.parseAndEval(input)

    actual should be(Success("6765"))
  }

  it should "compute addition" in {
    val input = "(+ 1 2)"
    val actual = TinyLisp.parseAndEval(input)
    actual should be(Success("3"))
  }

  it should "compute division" in {
    val input = "(/ 4 2)"
    val actual = TinyLisp.parseAndEval(input)
    actual should be(Success("2"))
  }

  it should "compute division with truncation" in {
    val input = "(/ 5 3)"
    val actual = TinyLisp.parseAndEval(input)
    actual should be(Success("1"))
  }

  it should "compute logical and" in {
    val input = "(and t nil t)"
    val actual = TinyLisp.parseAndEval(input)
    actual should be(Success("nil"))
  }

  it should "compute logical or" in {
    val input = "(or -3 nil t)"
    val actual = TinyLisp.parseAndEval(input)
    actual should be(Success("-3"))
  }

  it should "compute not" in {
    val input = "(not 123)"
    val actual = TinyLisp.parseAndEval(input)
    actual should be(Success("nil"))
  }

  it should "compute lower than" in {
    val input = "(< 1 2)"
    val actual = TinyLisp.parseAndEval(input)
    actual should be(Success("t"))
  }

  it should "compute equals" in {
    val input = "(= 0 0)"
    val actual = TinyLisp.parseAndEval(input)
    actual should be(Success("t"))
  }

  it should "compute conditional" in {
    val input = "(if (or nil t) 123 unbound)"
    val actual = TinyLisp.parseAndEval(input)
    actual should be(Success("123"))
  }
}
