package ch11.monad

import ch3.datastructures._
import ch4.errorhandling._
import ch5.strictnessandlaziness._
import ch6.functionalstate.State
import ch7.parallelism.Par.Par
import ch7.parallelism._
import ch8.propertytesting.Gen
import org.scalatest.{FunSuite, Matchers}

trait Monad[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def traverse[A, B](lma: List[F[A]])(f: A => B): F[List[B]] =
    lma.foldRight(unit(Nil: List[B]))((fa, b) => map2(fa, b)((a, r) => f(a) :: r))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(identity)

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))
}

object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A) = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A) = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma.flatMap(f)
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.lazyUnit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  // TODO how to create Either monad with two type parameters
//  val eitherMonad: Monad[Either] = new Monad[Either] {
//    override def unit[A](a: => A): Either[A, _] = Right(a)
//
//    override def flatMap[A, B](ma: Either[A, B])(f: A => Either[B]) = ???
//  }

  // as above
//  val stateMonad: Monad[State] = new Monad[State] {
//    override def unit[A](a: => A): State[_, A] = State.unit(a)
//
//    override def flatMap[A, B](ma: State[_ <: Nothing, A])(f: A => State[_ <: Nothing, B]): State[_ <: Nothing, B] =
//      ma.flatMap(f)
//  }
}

class MonoidTest extends FunSuite with Matchers {

  import Monad._

  test("sequence list test") {
    optionMonad.sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    optionMonad.sequence(List(Some(1), None)) shouldBe None
  }

  test("replicateM") {
    optionMonad.replicateM(3, Some(1)) shouldBe Some(List(1,1,1))

    optionMonad.replicateM(3, None) shouldBe None

    listMonad.replicateM(2, List(1)) shouldBe List(List(1, 1))
    listMonad.replicateM(2, List(1,2)) shouldBe List(List(1,1), List(1,2), List(2,1), List(2,2))
    listMonad.replicateM(2, Nil) shouldBe Nil
  }
}