package ch3.datastructures

import java.util.NoSuchElementException

import org.scalatest.{FunSuite, Matchers}

import scala.annotation.tailrec

trait List[+A] {
  def tail: List[A]

  def setHead[B >: A](b: B): List[B]

  def drop(n: Int): List[A]

  def dropWhile(f: A => Boolean): List[A]

  def init: List[A]

  def foldRight[B](z: B)(f: (A, B) => B): B

  def foldLeft[B](z: B)(f: (B, A) => B): B

  def lengthWithFoldRight: Int

  def lengthWithFoldLeft: Int

  def reverse: List[A]

  def appendWithFoldLeft[B >: A](l: List[B]): List[B]

  def appendWithFoldRight[B >: A](l: List[B]): List[B]

  def map[B](f: A => B): List[B]

  def filter(f: A => Boolean): List[A]

  def filterWithFlatMap(f: A => Boolean): List[A]

  def flatMap[B >: A](f: B => List[B]): List[B]

  def zipWith[B >: A](l : List[B])(f: (A, B) => B): List[B]

  def hasSubsequence[B >: A](l: List[B]): Boolean
}

case object Nil extends List[Nothing] {
  override def tail: List[Nothing] = throw new UnsupportedOperationException

  override def setHead[B](b: B): List[B] = throw new UnsupportedOperationException

  override def drop(n: Int): List[Nothing] = this

  override def dropWhile(f: (Nothing) => Boolean): List[Nothing] = this

  override def init: List[Nothing] = throw new NoSuchElementException

  override def foldRight[B](z: B)(f: (Nothing, B) => B): B = z

  override def foldLeft[B](z: B)(f: (B, Nothing) => B): B = z

  override def lengthWithFoldRight: Int = 0

  override def lengthWithFoldLeft: Int = 0

  override def reverse: List[Nothing] = this

  override def appendWithFoldLeft[B](l: List[B]): List[B] = l

  override def appendWithFoldRight[B](l: List[B]): List[B] = l

  override def map[B](f: (Nothing) => B): List[B] = Nil

  override def filter(f: (Nothing) => Boolean): List[Nothing] = Nil

  override def filterWithFlatMap(f: (Nothing) => Boolean): List[Nothing] = Nil

  override def flatMap[B >: Nothing](f: (B) => List[B]): List[B] = Nil

  override def zipWith[B >: Nothing](l: List[B])(f: (Nothing, B) => B): List[B] = Nil

  override def hasSubsequence[B >: Nothing](l: List[B]): Boolean = if (l == Nil) true else false
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

  override def lengthWithFoldRight: Int = foldRight(0)((_, b) => b + 1)

  override def lengthWithFoldLeft: Int = foldLeft(0)((b, _) => b + 1)

  override def reverse: List[A] = foldLeft(Nil: List[A])((b, a) => Cons(a, b))

  override def appendWithFoldLeft[B >: A](l: List[B]): List[B] = reverse.foldLeft(l) { case (acc, c) =>
    Cons(c, acc)
  }

  override def appendWithFoldRight[B >: A](l: List[B]): List[B] = foldRight(l) { case (c, acc) => Cons(c, acc) }

  override def map[B](f: A => B): List[B] = Cons(f(head), tail.map(f))

  override def filter(f: (A) => Boolean): List[A] = if (f(head)) Cons(head, tail.filter(f)) else tail.filter(f)

  override def filterWithFlatMap(f: (A) => Boolean): List[A] = flatMap(a => if (f(a)) Cons(a, Nil) else Nil)

  override def flatMap[B >: A](f: (B) => List[B]): List[B] = List.concat(map(f))

  override def zipWith[B >: A](l: List[B])(f: (A, B) => B): List[B] = l match {
    case Cons(_head, _t) => Cons(f(head, _head), tail.zipWith(_t)(f))
    case Nil             => Nil
  }

  override def hasSubsequence[B >: A](l: List[B]): Boolean =
  {
    def startsWith(l1: List[B], l2: List[B]): Boolean = (l1, l2) match {
      case (_, Nil) => true
      case (Cons(head1, tail1), Cons(head2, tail2)) => if (head1 == head2) startsWith(tail1, tail2) else false
    }

    if (startsWith(this, l)) true else tail.hasSubsequence(l)
  }
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

  def productWithFoldRight(ds: List[Double]): Double = ds.foldRight(1.0)(_ * _)

  def productWithFoldLeft(ds: List[Double]): Double = ds.foldLeft(1.0)(_ * _)

  def sumWithFoldLeft(ds: List[Double]): Double = ds.foldLeft(0.0)(_ + _)

  def foldRight[A, B](ds: List[A], z: B)(f: (A, B) => B): B = ds match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @tailrec
  def foldLeft[A, B](ds: List[A], z: B)(f: (B, A) => B): B = ds match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldRight2[A, B](ds: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(ds.reverse, z)((a, b) => f(b, a))

  def foldLeft2[A, B](ds: List[A], z: B)(f: (B, A) => B): B =
    foldRight(ds.reverse, z)((a, b) => f(b, a))

  def :\[A, B](ds: List[A], z: B)(f: (A, B) => B): B = foldRight(ds, z)(f)

  def /:[A, B](ds: List[A], z: B)(f: (B, A) => B): B = foldLeft(ds, z)(f)

  def concat[A](lists: List[List[A]]): List[A] = foldLeft(lists, Nil: List[A]) { case (a, acc) =>
    a.appendWithFoldLeft(acc)
  }
}


class ListTest extends FunSuite with Matchers {

  import List._

  test("product") {
    testProduct(product)
  }

  test("productWithFoldRight") {
    testProduct(productWithFoldRight)
  }

  test("productWithFoldLeft") {
    testProduct(productWithFoldLeft)
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
    Nil.lengthWithFoldRight should be(0)
    List().lengthWithFoldRight should be(0)
    List(1).lengthWithFoldRight should be(1)
    List(1, 3, 5).lengthWithFoldRight should be(3)
  }

  test("reverse") {
    Nil.reverse should be(Nil)
    List(1).reverse should be(List(1))
    List(1, 3, 5).reverse should be(List(5, 3, 1))
  }

  test("append") {
    def appendWithFoldLeft[A](l1: List[A], l2: List[A]) = l1.appendWithFoldLeft(l2)
    def appendWithFoldRight[A](l1: List[A], l2: List[A]) = l1.appendWithFoldRight(l2)

    def testAppend(append: (List[Int], List[Int]) => List[Int]) {
      append(List(), List()) should be(List())
      append(List(1), List()) should be(List(1))
      append(List(), List(1)) should be(List(1))
      append(List(1, 2), List(3, 4)) should be(List(1, 2, 3, 4))
    }

    testAppend(appendWithFoldLeft)
    testAppend(appendWithFoldRight)
  }

  test("concat") {
    concat(List(List())) should be(List())
    concat(List(List(1))) should be(List(1))
    concat(List(List(1, 2))) should be(List(1, 2))
    concat(List(List(1, 2), List(3))) should be(List(1, 2, 3))
    concat(List(List(1, 2), List(3), List(4))) should be(List(1, 2, 3, 4))
  }

  test("map") {
    List(1, 2, 3).map(_ + 1) should be(List(2, 3, 4))
    List(1.0, 2.0, 3.0).map(_.toString) should be(List("1.0", "2.0", "3.0"))
  }

  test("filter") {
    List(1, 2, 3).filter(_ > 2) should be(List(3))
    List(1, 2, 3).filter(_ <= 2) should be(List(1, 2))
  }

  test("filterWithFlatMap") {
    List(1, 2, 3).filterWithFlatMap(_ > 2) should be(List(3))
    List(1, 2, 3).filterWithFlatMap(_ <= 2) should be(List(1, 2))
  }

  test("flatmap") {
    List(1, 2, 3).flatMap(a => List(a, a)) should be(List(1, 1, 2, 2, 3, 3))
  }

  test("zipWith") {
    List(1, 2, 3).zipWith(List())(_ + _) should be (List())
    List(1, 2, 3).zipWith(List(1, 2))(_ + _) should be (List(2, 4))
    List(1, 2, 3).zipWith(List(3, 4, 5))(_ + _) should be (List(4, 6, 8))
    List(1).zipWith(List(1, 2))(_ + _) should be (List(2))
    List[Int]().zipWith(List(1, 2))(_ + _) should be (List())
  }

  test("has subsequence") {
    List().hasSubsequence(List()) should be(true)
    List().hasSubsequence(List(1)) should be(false)
    List(1).hasSubsequence(List(1)) should be(true)
    List(2).hasSubsequence(List(1)) should be(false)

    val l = List(1, 2, 3, 4)
    l.hasSubsequence(List()) should be (true)
    l.hasSubsequence(List(1)) should be (true)
    l.hasSubsequence(List(2)) should be (true)
    l.hasSubsequence(List(1, 2)) should be (true)
    l.hasSubsequence(List(1, 2, 3)) should be (true)
    l.hasSubsequence(List(1, 2, 4)) should be (false)
    l.hasSubsequence(List(1, 2, 3, 4)) should be (true)
    l.hasSubsequence(List(2, 3, 4)) should be (true)
    l.hasSubsequence(List(3, 4)) should be (true)
    l.hasSubsequence(List(4)) should be (true)
    l.hasSubsequence(List(3)) should be (true)
  }
}
