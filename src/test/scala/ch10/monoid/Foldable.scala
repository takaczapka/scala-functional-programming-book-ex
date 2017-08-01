package ch10.monoid

import ch10.monoid.Monoid.{intAddition, mergeMapMonoid}
import ch3.datastructures.{List, Tree}
import ch4.errorhandling.Option
import ch5.strictnessandlaziness.Stream
import org.scalatest.{FunSuite, Matchers}

trait Foldable[F[_]] {

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B

  def concatenate[B](as: F[B])(m: Monoid[B]): B = foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] = foldRight(fa)(List[A]())((a, b) => a :: b)
}

object ListFoldable extends Foldable[List] {

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = List.foldLeft(as, z)(f)

  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = List.foldRight(as, z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B = foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))
}

object StreamFoldable extends Foldable[Stream] {

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)((a, b) => f(a, b))

  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)((a, b) => f(a, b))

  override def foldMap[A, B](as: Stream[A])(f: (A) => B)(m: Monoid[B]): B =
    as.foldRight(m.zero)((a, b) => m.op(f(a), b))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B = foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))
}


// NOTE, FIP examples says:
// Notice that in `TreeFoldable.foldMap`, we don't actually use the `zero`
// from the `Monoid`. This is because there is no empty tree.
// This suggests that there might be a class of types that are foldable
// with something "smaller" than a monoid, consisting only of an
// associative `op`. That kind of object (a monoid without a `zero`) is
// called a semigroup. `Tree` itself is not a monoid, but it is a semigroup.

// Implementation here ignores it and treats Tree as List of leaf nodes when folding
object TreeFoldable extends Foldable[Tree] {
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as.toList.foldLeft(z)(f)

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as.toList.foldRight(z)(f)

  override def foldMap[A, B](as: Tree[A])(f: (A) => B)(m: Monoid[B]): B = as.fold(f)(m.op)
}

object OptionFoldable extends Foldable[Option] {
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as.toList.foldLeft(z)(f)

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = foldLeft(as)(z)((a, b) => f(b, a))

  override def foldMap[A, B](as: Option[A])(f: (A) => B)(m: Monoid[B]): B = foldLeft(as.map(f))(m.zero)(m.op)
}

object Foldable {
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = IndexedSeqFoldable.foldMap(as)(a => Map(a -> 1))(mergeMapMonoid(intAddition))
}

class FoldableTest extends FunSuite with Matchers {
  test("stream fold map") {
    StreamFoldable.foldMap(Stream(1, 2, 3, 4))(identity)(Monoid.intAddition) shouldBe 10
  }

  test("bag") {
    import Foldable._

    bag(IndexedSeq(1, 1, 2, 3, 4, 1, 2)) shouldBe Map(1 -> 3, 2 -> 2, 3 -> 1, 4 -> 1)
  }
}