package ch3.datastructures

import org.scalatest.{FunSuite, Matchers}

sealed trait Tree[+A] {
  def size: Int

  def sizeWithFold: Int

  def maximum[B >: A](implicit cmp: Ordering[B]): A

  def depth: Int

  def depthWithFold: Int

  def map[B](f: A => B): Tree[B]

  def mapWithFold[B](f: A => B): Tree[B]

  def fold[B](f: A => B)(g: (B, B) => B): B

  def toList: List[A]
}

case class Leaf[A](v: A) extends Tree[A] {
  override def size: Int = 1

  override def sizeWithFold: Int = 1

  def maximum[B >: A](implicit cmp: Ordering[B]): A = v

  override def depth: Int = 1

  override def depthWithFold: Int = 1

  override def map[B](f: (A) => B): Tree[B] = Leaf(f(v))

  override def mapWithFold[B](f: (A) => B): Tree[B] = Leaf(f(v))

  override def fold[B](f: A => B)(g: (B, B) => B): B = f(v)

  override def toList: List[A] = List(v)
}

case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def size: Int = left.size + right.size

  override def sizeWithFold: Int = fold(_ => 1)(_ + _)

  def maximum[B >: A](implicit cmp: Ordering[B]): A = fold(a => a){case (l, r) => if (cmp.gteq(l, r)) l else r }

  override def depth: Int = 1 + (left.depth max right.depth)

  override def depthWithFold: Int = fold(_ => 1) { (b1, b2) => 1 + (b1 max b2) }

  override def map[B](f: (A) => B): Tree[B] = Node(left.map(f), right.map(f))

  override def mapWithFold[B](f: (A) => B): Tree[B] = fold(a => Leaf(f(a)): Tree[B]) { case (l, r) => Node(l, r) }

  override def fold[B](f: A => B)(g: (B, B) => B): B = g(left.fold(f)(g), right.fold(f)(g))

  override def toList: List[A] = left.toList ++ right.toList
}


object Tree {
  def fold[A, B](t: Tree[A], z: B)(f: (A, B) => B): B = t match {
    case Leaf(v) => f(v, z)
    case Node(left, right) => fold(right, fold(left, z)(f))(f)
  }
}

class TreeTest extends FunSuite with Matchers {
  test("size") {
    Leaf(1).size should be(1)
    Node(Leaf(1), Leaf(2)).size should be(2)
    Node(Leaf(1), Node(Leaf(2), Leaf(3))).size should be(3)
  }

  test("sizeWithFold") {
    Leaf(1).sizeWithFold should be(1)
    Node(Leaf(1), Leaf(2)).sizeWithFold should be(2)
    Node(Leaf(1), Node(Leaf(2), Leaf(3))).sizeWithFold should be(3)
  }

  test("maximum") {
    Leaf(1).maximum should be(1)
    Node(Leaf(1), Leaf(2)).maximum should be(2)
    Node(Leaf(1), Node(Leaf(2), Leaf(3))).maximum should be(3)
    Node(Leaf(7), Node(Node(Leaf(1), Leaf(5)), Leaf(3))).maximum should be(7)
  }

  test("depth") {
    Leaf(1).depth should be(1)
    Node(Leaf(1), Leaf(2)).depth should be(2)
    Node(Leaf(1), Node(Leaf(2), Leaf(3))).depth should be(3)
    Node(Leaf(1), Node(Node(Leaf(3), Leaf(5)), Leaf(3))).depth should be(4)
  }

  test("depthWithFold") {
    Leaf(1).depthWithFold should be(1)
    Node(Leaf(1), Leaf(2)).depthWithFold should be(2)
    Node(Leaf(1), Node(Leaf(2), Leaf(3))).depthWithFold should be(3)
    Node(Leaf(1), Node(Node(Leaf(3), Leaf(5)), Leaf(3))).depthWithFold should be(4)
  }

  test("map") {
    Leaf(1).map(_ + 1) should be(Leaf(2))
    Node(Leaf(1), Node(Leaf(2), Leaf(3))).map(_ + 2) should be(Node(Leaf(3), Node(Leaf(4), Leaf(5))))
  }

  test("mapWithFold") {
    Leaf(1).mapWithFold(_ + 1) should be(Leaf(2))
    Node(Leaf(1), Node(Leaf(2), Leaf(3))).mapWithFold(_ + 2) should be(Node(Leaf(3), Node(Leaf(4), Leaf(5))))
  }

  test("toList") {
    Leaf(1).toList should be(List(1))
    Node(Leaf(1), Node(Leaf(2), Leaf(3))).toList should be(List(1, 2, 3))
  }
}

