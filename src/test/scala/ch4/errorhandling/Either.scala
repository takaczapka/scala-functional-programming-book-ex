package ch4.errorhandling

import org.scalatest.{FunSuite, Matchers}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(l) => Left(l)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]) : Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(l) => Left(l)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => Right(v)
    case Left(l) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap (aa => b.map(bb => f(aa, bb)))

  def isLeft: Boolean = this match {
    case Right(_) => false
    case _ => true
  }

  def isRight: Boolean = this match {
    case Left(_) => false
    case _ => true
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case head :: tail => f(head) match {
      case Left(e) => Left(e)
      case r @ Right(_) => r.map2(traverse(tail)(f))(_ :: _)
    }
  }
}

import Either._

class EitherTest extends FunSuite with Matchers {
  test("sequence") {
    sequence(List(Left(1))) should be (Left(1))
    sequence(List(Right(1))) should be (Right(List(1)))
    sequence(List(Right(1), Right(2))) should be (Right(List(1, 2)))
    sequence(List(Right(1), Right(2), Left(1))) should be (Left(1))
    sequence(List(Right(1), Left(1), Right(2), Left(2))) should be (Left(1))
  }
}