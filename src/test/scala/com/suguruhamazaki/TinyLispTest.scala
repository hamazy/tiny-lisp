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
}
