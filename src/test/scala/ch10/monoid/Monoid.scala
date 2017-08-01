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

    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
  }

  def endoMonoid[A] = new Monoid[A => A] {
    override def zero: A => A = identity

    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2
  }

  // dual monoid is any monoid just by flipping the `op`.
  // very monoid has a _dual_ where the
  // `op` combines things in the opposite order. Monoids like `booleanOr` and
  // `intAddition` are equivalent to their duals because their `op` is commutative
  // as well as associative, but option is not commutative so there are two correct implementations
  def dual[A](a: Monoid[A]) = new Monoid[A] {
    override def zero: A = a.zero

    override def op(a1: A, a2: A): A = a.op(a2, a1)
  }

  def product[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

    override def zero: (A, B) = (A.zero, B.zero)
  }

  def mergeMapMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def zero: Map[K, V] = Map()

    // classic
    //    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] = (a1.keys ++ a2.keys).map(
    //      k => k -> V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero))).toMap

    // with fold left
    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] = (a1.keys ++ a2.keys).foldLeft(zero)(
      (acc, k) => acc.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero))))
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def zero: A => B = _ => B.zero

    override def op(a1: A => B, a2: A => B): A => B = a => B.op(a1(a), a1(a))
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll("associativity", Gen.listOfN(3, gen)) { case List(a, b, c) => m.op(a, m.op(b, c)) == m.op(m.op(a, b), c) } &&
      Prop.forAll("zero", gen) { a => m.op(a, m.zero) == a && m.op(m.zero, a) == a }

  // FOLDING OVER MONOIDS
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)((b, a) => m.op(b, a))

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldLeftWithFoldMap[A, B](as: List[A], m: Monoid[B])(z: B)(f: (B, A) => B): B =
    foldMap(as, m)(a => f(z, a))


  // folding by splitting the seq in two
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = as match {
    case IndexedSeq() => m.zero
    case IndexedSeq(elem) => f(elem)
    case other => {
      val (left, right) = other.splitAt(other.size / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  // TODO implement parallel version of foldMapV using par


  val strange = new Monoid[Option[(Int, Int, Boolean)]] { // TODO
      override def zero: Option[(Int, Int, Boolean)] = None

    override def op(l: Option[(Int, Int, Boolean)], r: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
      (l, r) match {
        case (x, None) => None
        case (None, x) => None
        case (Some((a1, a2, isOr1)), Some((b1, b2, isOr2))) =>
          if (!isOr1 || !isOr2) None else
          Some {
            (a1 min b1, a2 max b2, a1 <= b1 && a2 <= b2 )
          }

      }
  }

  // Use foldMap to detect whether a given IndexedSeq[Int] is ordered
  // TODO
  def isOrdered(as: IndexedSeq[Int]): Boolean = foldMapV(as, strange)(a => Some((a, a, true))).map(_._3).getOrElse(false)
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

  test("foldMapV") {
    val as = IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    foldMapV(as, intAddition)(identity) shouldBe 81
  }

  test("strange") {
    Prop.runResult(monoidLaws(strange, Gen.int.map(_ % 10).map(i => Option((i, i, true))))) shouldBe Passed
  }

  test("isOrdered") {
    isOrdered(IndexedSeq(1, 2, 3)) shouldBe true
    isOrdered(IndexedSeq(1, 2, 2)) shouldBe true
    isOrdered(IndexedSeq(2, 2, 2)) shouldBe true
    isOrdered(IndexedSeq(2, 1, 2)) shouldBe false
    isOrdered(IndexedSeq(2, 1, 1)) shouldBe false
    isOrdered(IndexedSeq(2, 1, 0)) shouldBe false

    isOrdered(IndexedSeq(-542078577, -834216340, 473868580)) shouldBe false
  }
}