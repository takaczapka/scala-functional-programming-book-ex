package ch5.strictnessandlaziness

import ch5.strictnessandlaziness.Stream._
import org.scalatest.{FunSuite, Matchers}

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
  def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def existsWithFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileWithFoldRight(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else Empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a).orElse(b))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](l: Stream[B]): Stream[B] = foldRight(l)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a).append(b))

  def mapWithUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case Empty => None
  }

  def takeWithUnfold(n: Int): Stream[A] = unfold((n, this)) {
    case (a, Cons(h, t)) if a > 0 => Some(h(), (a - 1, t()))
    case _ => None
  }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B >: A](l: Stream[B])(f: (A, B) => B): Stream[B] = unfold((this, l)) {
    case (Empty, Empty) => None
    case (_, Empty) => None
    case (Empty, _) => None
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
  }

  def zipAll[B](l: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, l)) {
    case (Empty, Empty) => None
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
  }

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).foldRight(true)((curr, res) =>
    curr match {
      case (_, None) => true
      case (Some(a), Some(b)) if a == b => res
      case _ => false
    }
  )

  def tails: Stream[Stream[A]] = unfold(this) {
    case all@Cons(h1, t1) => Some((all, t1()))
    case _ => None
  } append Stream(empty[A])

  def hasSubsequence[B >: A](l: Stream[B]): Boolean =
    tails startsWith l

  // not very efficient
  def scanRightWithTails[B](z: B)(f: (A, => B) => B): Stream[B] = tails.map(_.foldRight(z)(f))

  // much better
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight((z, Stream(z)))((a, b) =>
    b match {
      case (_z, stream) =>
        val c = f(a, _z)
        (c, cons(c, stream))
    }
  )._2
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

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(i: Int): Stream[Int] = cons(i, from(i + 1))

  def fibs(): Stream[Int] = {
    def fib(x: Int, y: Int): Stream[Int] = cons(y, fib(y, x + y))

    cons(0, fib(0, 1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty[A]
  }

  def fibsWithUnfold(): Stream[Int] = {
    def fib(p: (Int, Int)): Option[(Int, (Int, Int))] = Some(p._1, (p._2, p._1 + p._2))

    unfold((0, 1))(fib)
  }

  def fromWithUnfold(i: Int): Stream[Int] = {
    unfold(i)(s => Some((s, s + 1)))
  }

  def constantWithUnfold(i: Int): Stream[Int] = {
    unfold(i)(s => Some((s, s)))
  }

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
    Stream(1, 2, 3).exists(_ == 2) should be(true)
    Stream(1, 2, 3).existsWithFoldRight(_ == 2) should be(true)

    Stream(1, 2, 3).exists(_ == 4) should be(false)
    Stream(1, 2, 3).existsWithFoldRight(_ == 4) should be(false)
  }

  test("headOption") {
    Stream().headOption should be(None)
    Stream(1).headOption should be(Some(1))
    Stream(1, 2).headOption should be(Some(1))
    Stream(1, 2, 3).headOption should be(Some(1))
  }

  test("map") {
    Stream[Int]().map(_ + 1).toList should be(Nil)
    Stream(1).map(_ + 1).toList should be(List(2))
    Stream(1, 2).map(_ + 1).toList should be(List(2, 3))
    Stream(1, 2, 3).map(_ + 1).toList should be(List(2, 3, 4))
  }

  test("filter") {
    Stream[Int]().filter(_ > 1).toList should be(Nil)
    Stream(1).filter(_ > 1).toList should be(Nil)
    Stream(1, 2).filter(_ > 1).toList should be(List(2))
    Stream(1, 2, 3).filter(_ > 1).toList should be(List(2, 3))
  }

  test("append") {
    Stream[Int]().append(Stream(1)).toList should be(List(1))
    Stream(1).append(Stream(1)).toList should be(List(1, 1))
    Stream(1, 2).append(Stream(1)).toList should be(List(1, 2, 1))
    Stream(1, 2, 3).append(Stream(1)).toList should be(List(1, 2, 3, 1))
    Stream(1, 2, 3).append(Stream(1, 5)).toList should be(List(1, 2, 3, 1, 5))
  }

  test("flatMap") {
    Stream[Int]().flatMap(i => Stream(i, i)).toList should be(Nil)
    Stream(1).flatMap(i => Stream(i, i)).toList should be(List(1, 1))
    Stream(1, 2).flatMap(i => Stream(i, i)).toList should be(List(1, 1, 2, 2))
    Stream(1, 2, 3).flatMap(i => Stream(i, i)).toList should be(List(1, 1, 2, 2, 3, 3))
  }

  test("constant") {
    constant(6).take(3).toList should be(List(6, 6, 6))
  }

  test("from") {
    from(6).take(3).toList should be(List(6, 7, 8))
  }

  test("fibs") {
    fibs().take(9).toList should be(List(0, 1, 1, 2, 3, 5, 8, 13, 21))
  }

  test("unfold") {
    def f(i: Int) = Some((i, i + 1))

    unfold(5)(f).take(5).toList should be(List(5, 6, 7, 8, 9))
  }

  test("fibsWithUnfold") {
    fibsWithUnfold().take(9).toList should be(List(0, 1, 1, 2, 3, 5, 8, 13, 21))
  }

  test("constantWithUnfold") {
    constantWithUnfold(6).take(3).toList should be(List(6, 6, 6))
  }

  test("fromWithUnfold") {
    fromWithUnfold(6).take(3).toList should be(List(6, 7, 8))
  }

  test("mapWithUnfold") {
    Stream[Int]().mapWithUnfold(_ + 1).toList should be(Nil)
    Stream(1).mapWithUnfold(_ + 1).toList should be(List(2))
    Stream(1, 2).mapWithUnfold(_ + 1).toList should be(List(2, 3))
    Stream(1, 2, 3).mapWithUnfold(_ + 1).toList should be(List(2, 3, 4))
  }

  test("takeWithUnfold") {
    Stream().takeWithUnfold(0).toList should be(Empty.toList)
    Stream().takeWithUnfold(1).toList should be(Empty.toList)
    Stream(1).takeWithUnfold(0).toList should be(Empty.toList)
    Stream(1).takeWithUnfold(2).toList should be(Stream(1).toList)
    Stream(1, 3).takeWithUnfold(2).toList should be(Stream(1, 3).toList)
  }

  test("takeWhileWithUnfold") {
    Stream[Int]().takeWhileWithUnfold(_ > 1).toList should be(Nil)
    Stream[Int]().takeWhileWithUnfold(_ > 1).toList should be(Nil)
    Stream(1).takeWhileWithUnfold(_ > 0).toList should be(List(1))
    Stream(1).takeWhileWithUnfold(_ > 1).toList should be(Nil)
    Stream(1, 3).takeWhileWithUnfold(_ > 0).toList should be(List(1, 3))
    Stream(1, 3).takeWhileWithUnfold(_ > 1).toList should be(Nil)
    Stream(1, 3).takeWhileWithUnfold(_ < 3).toList should be(List(1))
    Stream(1, 3).takeWhileWithUnfold(_ > 3).toList should be(Nil)
  }

  test("zipWith") {
    Stream(1, 2, 3).zipWith(Stream())(_ + _).toList should be(List())
    Stream(1, 2, 3).zipWith(Stream(1, 2))(_ + _).toList should be(List(2, 4))
    Stream(1, 2, 3).zipWith(Stream(3, 4, 5))(_ + _).toList should be(List(4, 6, 8))
    Stream(1).zipWith(Stream(1, 2))(_ + _).toList should be(List(2))
    Stream[Int]().zipWith(Stream(1, 2))(_ + _).toList should be(List())
  }

  test("zipAll") {
    Stream(1, 2, 3).zipAll(Stream()).toList should be(List((Some(1), None), (Some(2), None), (Some(3), None)))
    Stream(1, 2, 3).zipAll(Stream(1, 2)).toList should be(List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), None)))
    Stream(1, 2, 3).zipAll(Stream(3, 4, 5)).toList should be(List((Some(1), Some(3)), (Some(2), Some(4)), (Some(3), Some(5))))
    Stream(1).zipAll(Stream(1, 2)).toList should be(List((Some(1), Some(1)), (None, Some(2))))
    Stream[Int]().zipAll(Stream(1, 2)).toList should be(List((None, Some(1)), (None, Some(2))))
  }

  test("startsWith") {
    Stream(1, 2, 3).startsWith(Stream()) should be(true)
    Stream(1, 2, 3).startsWith(Stream(1, 2)) should be(true)
    Stream(1, 2, 3).startsWith(Stream(3, 4, 5)) should be(false)
    Stream(1, 2, 3).startsWith(Stream(1, 3)) should be(false)
    Stream(1).startsWith(Stream(1, 2)) should be(false)
    Stream[Int]().startsWith(Stream(1, 2)) should be(false)
    ones.startsWith(Stream(1, 1, 1, 1, 1, 1)) should be(true)
  }

  test("tails") {
    Stream(1, 2, 3).tails.map(_.toList).toList should be(List(List(1, 2, 3), List(2, 3), List(3), List()))
  }

  test("scanRight") {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList should be(List(6, 5, 3, 0))
  }

}