package ch2.gettingstarted

import org.scalatest.{FunSuite, Matchers}

import scala.annotation.tailrec

/**
  * Implement tail recursive fibonacci sequence calculator.
  */
object Fib {

  def fib(n: Integer): Integer = {

    @tailrec
    def loop(f_n_1: Integer, f_n_2: Integer, current: Integer, n: Integer): Integer = {
      if (current == n) f_n_1 + f_n_2
      else loop(f_n_1 + f_n_2, f_n_1, current + 1, n)
    }
    if (n <= 0) 0
    else
      loop(0, 1, 1, n)
  }
}

class FibTest extends FunSuite with Matchers {

  import Fib._

  test("can calculate") {
    fib(0) should be(0)
    fib(1) should be(1)
    fib(2) should be(1)
    fib(5) should be(5)
    fib(10) should be(55)
    fib(12) should be(144)
  }
}
