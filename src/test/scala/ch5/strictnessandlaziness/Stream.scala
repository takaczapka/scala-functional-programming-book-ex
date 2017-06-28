package ch5.strictnessandlaziness

import org.scalatest.{FunSuite, Matchers}

import Stream._

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  // not => B --- if it's not called it will not evaluate so it's a foldRight with a stop!
  def foldRight[B](z: B)(f: (A, =>B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def existsWithFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileWithFoldRight(f: A => Boolean): Stream[A] =
    foldRight(Empty : Stream[A])((a, b) => if (f(a)) cons(a, b) else Empty )

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a).orElse(b))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  def apply[A](a: A*): Stream[A] = if (a.isEmpty) Empty else cons(a.head, apply(a.tail: _*))

  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val h = head
    lazy val t = tail
    Cons(() => h, () => t)
  }

  def empty[A]: Stream[A] = Empty
}

class StreamTest extends FunSuite with Matchers {
  test("toList") {
    Stream().toList should be(Nil)
    Stream(1).toList should be(List(1))
    Stream(1, 3).toList should be(List(1, 3))
  }

  test("take") {
    Stream().take(0).toList should be(Empty.toList)
    Stream().take(1).toList should be(Empty.toList)
    Stream(1).take(0).toList should be(Empty.toList)
    Stream(1).take(2).toList should be(Stream(1).toList)
    Stream(1, 3).take(2).toList should be(Stream(1, 3).toList)
  }

  test("drop") {
    Stream().drop(0).toList should be(Empty.toList)
    Stream().drop(1).toList should be(Empty.toList)
    Stream(1).drop(0).toList should be(Stream(1).toList)
    Stream(1).drop(2).toList should be(Empty.toList)
    Stream(1, 3).drop(0).toList should be(Stream(1, 3).toList)
    Stream(1, 3).drop(1).toList should be(Stream(3).toList)
    Stream(1, 3).drop(2).toList should be(Stream().toList)
  }

  test("takeWhile") {
    Stream[Int]().takeWhile(_ > 1).toList should be(Nil)
    Stream[Int]().takeWhile(_ > 1).toList should be(Nil)
    Stream(1).takeWhile(_ > 0).toList should be(List(1))
    Stream(1).takeWhile(_ > 1).toList should be(Nil)
    Stream(1, 3).takeWhile(_ > 0).toList should be(List(1, 3))
    Stream(1, 3).takeWhile(_ > 1).toList should be(Nil)
    Stream(1, 3).takeWhile(_ < 3).toList should be(List(1))
    Stream(1, 3).takeWhile(_ > 3).toList should be(Nil)
  }

  test("takeWhileWithFoldRight") {
    Stream[Int]().takeWhileWithFoldRight(_ > 1).toList should be(Nil)
    Stream[Int]().takeWhileWithFoldRight(_ > 1).toList should be(Nil)
    Stream(1).takeWhileWithFoldRight(_ > 0).toList should be(List(1))
    Stream(1).takeWhileWithFoldRight(_ > 1).toList should be(Nil)
    Stream(1, 3).takeWhileWithFoldRight(_ > 0).toList should be(List(1, 3))
    Stream(1, 3).takeWhileWithFoldRight(_ > 1).toList should be(Nil)
    Stream(1, 3).takeWhileWithFoldRight(_ < 3).toList should be(List(1))
    Stream(1, 3).takeWhileWithFoldRight(_ > 3).toList should be(Nil)
    Stream(1, 2, 3, 3, 5).takeWhileWithFoldRight(_ < 5).toList should be(List(1, 2, 3, 3))
  }

  test("exists") {
    Stream(1, 2, 3).exists(_ == 2) should be (true)
    Stream(1, 2, 3).existsWithFoldRight(_ == 2) should be (true)

    Stream(1, 2, 3).exists(_ == 4) should be (false)
    Stream(1, 2, 3).existsWithFoldRight(_ == 4) should be (false)
  }

  test("headOption") {
    Stream().headOption should be(None)
    Stream(1).headOption should be(Some(1))
    Stream(1, 2).headOption should be(Some(1))
    Stream(1, 2, 3).headOption should be(Some(1))
  }
}