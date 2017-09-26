package additional

class Empty[A] {
  def map[B](f: A => B) = new Empty[B]()
  def flatMap[B](f: A => Empty[B]) = new Empty[B]()
}

case class Identity[A](a: A) {
  def map[B](f: A => B) = new Identity[B](f(a))
  def flatMap[B](f: A => Identity[B]) = f(a)
}

case class Lazy[A](apply: () => A) { self =>
  def map[B](f: A => B) = new Lazy[B](() => f(apply()))
  def flatMap[B](f: A => Lazy[B]): Lazy[B] = Lazy[B](() => f(apply()).apply())

//  def toIdle: Idle[A] = () => self.apply()
}

//case class Idle[A] { self =>
//  def apply(): A
//
//  def map[B](f: A => B): Idle[B] = new Idle[B](() => f(self.apply()))
//  def flatMap[B](f: A => Idle[B]): Idle[B] = () => f(self.apply()).apply()
//
//  def toLazy: Lazy[A] = Lazy[A](apply)
//}

sealed trait Tree[A] {
  def map[B](f: A => B): Tree[B]
  def flatMap[B](f: A => Tree[B]): Tree[B]
}

case class Leaf[A](a: A) extends Tree[A] {
  def map[B](f: A => B): Leaf[B] = Leaf(f(a))
  def flatMap[B](f: A => Tree[B]): Tree[B] = f(a)
}

case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  def map[B](f: A => B): Node[B] = Node(left.map(f), right.map(f))
  def flatMap[B](f: A => Tree[B]): Tree[B] = Node[B](left.flatMap(f), right.flatMap(f))
}