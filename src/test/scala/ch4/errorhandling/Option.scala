package ch4.errorhandling

import org.scalatest.{FunSuite, Matchers}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(get) => Some(f(get))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(get) => get
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

}

case object None extends Option[Nothing]

case class Some[+A](get: A) extends Option[A]

object Option {
  def variance(xs: Seq[Double]): Option[Double] = {
    if (xs.size <= 1) None
    else {
      val mean = xs.sum / xs.length
      val powed = xs.map(x => math.pow(x - mean, 2))
      Some(powed.sum / (powed.length - 1))
    }
  }

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def meanMinusOne(xs: Seq[Double]): Option[Double] = if (xs.size <= 1) None else Some(xs.sum / xs.length - 1)

  def varianceWithFlatMap(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => meanMinusOne(xs.map(x => math.pow(x - m, 2))))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aa => b.map(bb => f(aa, bb)))
  }

  // into Option containing list of some values
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(List()): Option[List[A]]) { case (n, b) =>
      map2(n, b)(_ :: _)
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(List()): Option[List[B]]) { case (n, b) =>
      map2(f(n), b)(_ :: _)
    }

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case head :: tail => map2(f(head), traverse2(tail)(f))(_ :: _)
  }

  // this one finishes early - on first None
  def traverse3[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case head :: _ if f(head) == None => None
    case head :: tail => map2(f(head), traverse2(tail)(f))(_ :: _)
  }

  def sequenceWithTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse3(a)(identity)
}

import ch4.errorhandling.Option._

class OptionTest extends FunSuite with Matchers {
  test("variance") {
    variance(Seq.empty) should be(None)
    variance(Seq(1)) should be(None)
    variance(Seq(1, 1)) should be(Some(0.0))
    variance(Seq(1, 2)) should be(Some(0.5))
    variance(Seq(1, 2, 3)) should be(Some(1.0))
  }

  test("varianceWithFlatMap") {
    varianceWithFlatMap(Seq.empty) should be(None)
    varianceWithFlatMap(Seq(1)) should be(Some(None))
    varianceWithFlatMap(Seq(1, 1)) should be(Some(0.0))
    varianceWithFlatMap(Seq(1, 2)) should be(Some(0.5))
    varianceWithFlatMap(Seq(1, 2, 3)) should be(Some(1.0))
  }

  test("lift") {
    def f = (i: Int) => i * i

    val lifted: (Option[Int]) => Option[Int] = lift(f)

    lifted(Some(2)) should be(Some(4))
  }

  test("sequence") {
    sequence(List(None)) should be(None)
    sequence(List(Some(1))) should be(Some(List(1)))
    sequence(List(Some(1), None, Some(2))) should be(None)
    sequence(List(None, None, None)) should be(None)
    sequence(List(Some(1), Some(2), Some(3))) should be(Some(List(1, 2, 3)))
  }

  test("sequenceWithTraverse") {
    sequenceWithTraverse(List(None)) should be(None)
    sequenceWithTraverse(List(Some(1))) should be(Some(List(1)))
    sequenceWithTraverse(List(Some(1), None, Some(2))) should be(None)
    sequenceWithTraverse(List(None, None, None)) should be(None)
    sequenceWithTraverse(List(Some(1), Some(2), Some(3))) should be(Some(List(1, 2, 3)))
  }

  test("traverse") {
    def f(i : Int) = if (i > 0) Some(i) else None

    traverse(List())(f) should be(Some(List()))
    traverse(List(1))(f) should be(Some(List(1)))
    traverse(List(1, 0, 2))(f) should be(None)
    traverse(List(0, -1))(f) should be(None)
    traverse(List(1, 2, 3))(f) should be(Some(List(1, 2, 3)))
  }
}