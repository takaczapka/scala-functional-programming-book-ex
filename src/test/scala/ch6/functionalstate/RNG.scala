package ch6.functionalstate

import org.scalatest.{FunSuite, Matchers}

import scala.util.Random

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}


object RNG {

  def rng: RNG = SimpleRNG(Random.nextLong())
  def simpleRng = rng

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if (i >= 0) (i, r) else nonNegativeInt(r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (Math.abs(i - 1).toDouble / Int.MaxValue, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def ints(count: Int, acc: List[Int])(rng: RNG): (List[Int], RNG) = {
      if (count > 0) {
        val (i, r) = rng.nextInt
        ints(count - 1, i :: acc)(r)
      } else (acc, rng)
    }

    ints(count, Nil)(rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, r) = s(rng)
    (f(a), r)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def doubleRand: Rand[Double] = map(double)(i => i)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rc => {
    val (a, rrr1): (A, RNG) = ra(rc)
    val (b, rrr2): (B, RNG) = rb(rrr1)

    (f(a, b), rrr2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def randIntDoubleWithMap2: Rand[(Int, Double)] = both(int, double)

  def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, rr) = ra(rng)
    f(a)(rr)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = r => {
    fs.foldRight((Nil: List[A], r)) { case (rand, (res, rng)) =>
      val (a, nxtRng): (A, RNG) = rand(rng)
      (a :: res, nxtRng)
    }
  }

  def mapUsingFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2UsingFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

import ch6.functionalstate.RNG._

class RNGTest extends FunSuite with Matchers {
  test("RNG") {
    val rng: RNG = SimpleRNG(1)

    var newRng = rng
    0 to 5 foreach (_ => {
      val (i, nR) = newRng.nextInt
      println(i)
      newRng = nR
    })
  }

  test("nonNegativeRng") {
    (nonNegativeInt(rng)._1 >= 0) should be(true)
  }

  test("double") {
    val v = double(rng)
    (v._1 > 0 && v._1 < 1) should be(true)
  }

  test("ints") {
    ints(5)(rng)._1.size should be(5)
  }

  test("sequence") {
    val r: Rand[List[Int]] = sequence(List(int, int, int))
    println(r(rng)._1)
  }

  test("rollDie") {
    (0 to 10).foldLeft(rng) { case (rr, _) =>
      val (dice, newR) = rollDie(rr)
      println(dice)
      newR
    }
  }
}