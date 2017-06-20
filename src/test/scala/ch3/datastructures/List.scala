package ch3.datastructures

import java.util.NoSuchElementException

import org.scalatest.{FunSuite, Matchers}

trait List[+A] {
  def tail: List[A]

  def setHead[B >: A](b: B): List[B]

  def drop(n: Int): List[A]

  def dropWhile(f: A => Boolean): List[A]

  def init: List[A]

  def foldRight[B](z: B)(f: (A, B) => B): B

  def foldLeft[B](z: B)(f: (B, A) => B): B

  def length: Int
  def length2: Int

  def reverse: List[A]
}

case object Nil extends List[Nothing] {
  override def tail: List[Nothing] = throw new UnsupportedOperationException

  override def setHead[B](b: B): List[B] = throw new UnsupportedOperationException

  override def drop(n: Int): List[Nothing] = this

  override def dropWhile(f: (Nothing) => Boolean): List[Nothing] = this

  override def init: List[Nothing] = throw new NoSuchElementException

  override def foldRight[B](z: B)(f: (Nothing, B) => B): B = z

  override def foldLeft[B](z: B)(f: (B, Nothing) => B): B = z

  override def length: Int = 0
  override def length2: Int = 0

  override def reverse: List[Nothing] = this
}

case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def setHead[B >: A](b: B): List[B] = Cons(b, tail)

  override def drop(n: Int): List[A] = if (n > 0) tail.drop(n - 1) else this

  override def dropWhile(f: (A) => Boolean): List[A] = if (f(head)) tail.dropWhile(f) else this

  override def init: List[A] = this match {
    case Cons(a, Nil) => Nil
    case Cons(a, t) => Cons(a, t.init)
  }

  final override def foldRight[B](z: B)(f: (A, B) => B): B = f(head, tail.foldRight(z)(f))

  final override def foldLeft[B](z: B)(f: (B, A) => B): B = tail.foldLeft(f(z, head))(f)

  override def length: Int = foldRight(0)((_, b) => b + 1)

  override def length2: Int = foldLeft(0)((b, _) => b + 1)

  override def reverse: List[A] = foldLeft(Nil:List[A])((b, a) => Cons(a, b))
}

object List {
  def apply[A](a: A*): List[A] = if (a.isEmpty) Nil else Cons(a.head, apply(a.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, Nil) => x
    case Cons(h, t) => h * product(t)
  }

  def product2(ds: List[Double]): Double = ds.foldRight(1.0)(_ * _)

  def product3(ds: List[Double]): Double = ds.foldLeft(1.0)(_ * _)

  def sum2(ds: List[Double]): Double = ds.foldLeft(0.0)(_ + _)
}


class ListTest extends FunSuite with Matchers {

  import List._

  test("product") {
    testProduct(product)
  }

  test("product2") {
    testProduct(product2)
  }

  test("product3") {
    testProduct(product3)
  }

  def testProduct(p: List[Double] => Double): Unit = {
    p(List(0.0)) should be(0.0)
    p(List(0.0, 1)) should be(0.0)
    p(List(1, 0)) should be(0.0)
    p(List(1, 2)) should be(2.0)
    p(List(4, 2)) should be(8.0)
  }

  test("matching precedence") {
    (List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }) should be(3)
  }

  test("there is a sting in the tail") {
    List(1).tail should be(Nil)
    List(1, 2, 3).tail should be(List(2, 3))

    an[UnsupportedOperationException] should be thrownBy Nil.tail
  }

  test("head out") {
    List(1).setHead(2) should be(List(2))
    List(1, 3).setHead(2) should be(List(2, 3))
    List(1, 3).setHead(2.5) should be(List(2.5, 3))

    an[UnsupportedOperationException] should be thrownBy Nil.setHead(1)
  }

  test("drop it") {

    List(1, 2, 3).drop(0) should be(List(1, 2, 3))
    List(1, 2, 3).drop(1) should be(List(2, 3))
    List(1, 2, 3).drop(2) should be(List(3))
    List(1, 2, 3).drop(3) should be(Nil)

    List(1, 2, 3).drop(4) should be(Nil)
    Nil.drop(1) should be(Nil)
  }

  test("drop while") {

    List(1, 2, 3).dropWhile(_ <= 1) should be(List(2, 3))
    List(1, 2, 3).dropWhile(_ > 1) should be(List(1, 2, 3))
    List(1, 2, 3).dropWhile(_ > 0) should be(Nil)

    List[Int]().dropWhile(_ > 0) should be(Nil)
    Nil.dropWhile(_ => true) should be(Nil)
  }

  test("init") {

    List(1, 2, 3).init should be(List(1, 2))
    List(1, 2).init should be(List(1))
    List(1).init should be(Nil)

    a[NoSuchElementException] should be thrownBy Nil.init
  }

  test("data constructors with foldRight") {
    List(1, 2, 3).foldRight(Nil: List[Int])(Cons(_, _)) should be(List(1, 2, 3))
  }

  test("length") {
    Nil.length should be(0)
    List().length should be(0)
    List(1).length should be(1)
    List(1, 3, 5).length should be(3)
  }

  test("reverse") {
    Nil.reverse should be(Nil)
    List(1).reverse should be(List(1))
    List(1, 3, 5).reverse should be(List(5, 3, 1))
  }
}
