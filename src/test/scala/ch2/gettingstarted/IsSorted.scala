package ch2.gettingstarted

import org.scalatest.{FunSuite, ShouldMatchers}

object IsSorted {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    def isSortedInner(c: A, tt: Array[A]): Boolean = {
      if (tt.isEmpty) true
      else {
        val next = tt.head
        if (!ordered(c, next)) false
        else isSortedInner(next, tt.tail)
      }
    }

    if (as.length < 2) true
    else isSortedInner(as.head, as.tail)
  }
}

class IsSortedTest extends FunSuite with ShouldMatchers {

  import IsSorted.isSorted

  test("empty is always sorted ") {
    isSorted(Array(), (a: Int, b: Int) => a > b) should be(true)
    isSorted(Array(), (a: Int, b: Int) => a < b) should be(true)
  }

  test("single element array is always sorted") {
    isSorted(Array(1), (a: Int, b: Int) => a > b) should be(true)
    isSorted(Array("s"), (a: String, b: String) => a < b) should be(true)
  }

  test("is sorted follows the rules of the ordered function") {
    isSorted(Array(1, 2, 3), (a: Int, b: Int) => a > b) should be(false)
    isSorted(Array(1, 2, 2, 3), (a: Int, b: Int) => a <= b) should be(true)
    isSorted(Array(1, 2, 3), (a: Int, b: Int) => a < b) should be(true)
    isSorted(Array(1, 2, 3), (a: Int, b: Int) => a == b) should be(false)
  }
}
