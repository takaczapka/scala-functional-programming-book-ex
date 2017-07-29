package ch10.monoid

import ch8.propertytesting.{Gen, Passed, Prop}
import org.scalatest.{FunSuite, Matchers}

trait Monoid[A] {
  def zero: A

  def op(a1: A, a2: A): A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    override def zero: String = ""

    override def op(a1: String, a2: String): String = a1 + a2
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def zero: List[A] = Nil

    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  }

  val intAddition = new Monoid[Int] {
    override def zero: Int = 0

    override def op(a1: Int, a2: Int): Int = a1 + a2
  }

  val intMultiplication = new Monoid[Int] {
    override def zero: Int = 1

    override def op(a1: Int, a2: Int): Int = a1 * a2
  }

  val booleanOr = new Monoid[Boolean] {
    override def zero: Boolean = false

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  }

  val booleanAnd = new Monoid[Boolean] {
    override def zero: Boolean = true

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    override def zero: Option[A] = None

    override def op(a1: Option[A], a2: Option[A]): Option[A] = if (a1.isDefined) a1 else a2
  }

  def endoMonoid[A] = new Monoid[A => A] {
    override def zero: A => A = identity

    override def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll("associativity", Gen.listOfN(3, gen)) { case List(a, b, c) => m.op(a, m.op(b, c)) == m.op(m.op(a, b), c) } &&
      Prop.forAll("zero", gen) { a => m.op(a, m.zero) == a && m.op(m.zero, a) == a }

  // FOLDING OVER MONOIDS
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)((b, a) => m.op(b, a))

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldLeftWithFoldMap[A, B](as: List[A], m: Monoid[B])(z: B)(f: (B, A) => B): B =
    foldMap(as, m)(a => f(z, a))


}

import ch10.monoid.Monoid._

class MonoidTest extends FunSuite with Matchers {
  test("stringMonoid props") {
    Prop.runResult(monoidLaws(stringMonoid, Gen.string)) shouldBe Passed
  }

  test("listMonoid props") {
    Prop.runResult(monoidLaws(listMonoid[Int], Gen.list(Gen.int))) shouldBe Passed
  }

  test("intAddition props") {
    Prop.runResult(monoidLaws(intAddition, Gen.int)) shouldBe Passed
  }

  test("intMultiplication props") {
    Prop.runResult(monoidLaws(intMultiplication, Gen.int)) shouldBe Passed
  }

  test("booleanOr props") {
    Prop.runResult(monoidLaws(booleanOr, Gen.boolean)) shouldBe Passed
  }

  test("booleanAnd props") {
    Prop.runResult(monoidLaws(booleanAnd, Gen.boolean)) shouldBe Passed
  }

  test("optionMonoid props") {
    Prop.runResult(monoidLaws(optionMonoid[Int], Gen.option(Gen.int))) shouldBe Passed
  }

  test("endoMonoid props") {
    // TODO implement generator for endo functions - see end of Chapter 8
    //    Prop.runResult(monoidLaws(endoMonoid, Gen.???)) shouldBe Passed
  }

  test("foldLeft using foldMap") {
    val as = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    val classic = as.foldLeft(0)(_ + _)
    val withFoldMap = foldLeftWithFoldMap(as, intAddition)(0)(_ + _)

    classic shouldBe withFoldMap
  }
}