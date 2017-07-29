package ch8.propertytesting

import ch5.strictnessandlaziness.Stream
import ch6.functionalstate.{RNG, State}
import org.scalatest.{FunSuite, Matchers}

case class Gen[A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.map(f).flatMap(_.sample))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = for {
    a <- this
    b <- g
  } yield f(a, b)

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val sample = State(
      RNG.map(RNG.nonNegativeLessThan(stopExclusive - start))(_ + start)
    )
    Gen(sample)
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.map(RNG.int)(_ % 2 == 0)))

  def int: Gen[Int] = Gen(State(RNG.int))

  def positiveInt: Gen[Int] = int.map(Math.abs)

  def double: Gen[Double] = Gen(State(RNG.double))

  def list[A](g: Gen[A]): Gen[List[A]] = int.map(i => i % 10).flatMap(i => listOfN(i, g))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence((1 to n).map(_ => g.sample).toList))

  // notice use of List.fill(n)(f) to call a function f n times
  def listOfN2[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def listOfN2[A](size: Gen[Int], g: Gen[A]): Gen[List[A]] =
    size.flatMap(n => listOfN(n, g))

  def intTuple: Gen[(Int, Int)] = {
    val g1 = int
    val g2 = int

    Gen(for {
      i <- g1.sample
      j <- g2.sample
    } yield (i, j))
  }

  def option[A](g: Gen[A]): Gen[Option[A]] =
    Gen(boolean.sample.flatMap(v => {
      if (v) g.sample.map(Some(_)) else State(r => (None, r))
    }))

  def fromOption[A](g: Gen[Option[A]]): Gen[A] = {

    def toReal(s: State[RNG, Option[A]]): State[RNG, A] = {
      s.flatMap {
        case None => toReal(s)
        case Some(a) => State.unit(a)
      }
    }

    Gen(toReal(g.sample))
  }

  def string: Gen[String] = {
    val a: Gen[List[Int]] = listOfN(10, choose(97, 122)) // ints between 97 - 122
    Gen(a.sample.map(_.map(_.toChar).mkString))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(v => if (v) g1 else g2)

  def sequence[A](fs: List[Gen[A]]): Gen[List[A]] =
    fs.foldRight(unit(Nil: List[A]))((g, acc) => g.map2(acc)(_ :: _))
}

case class SGen[A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = SGen(i => forSize(i).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(i => forSize(i).map(f).flatMap(_.forSize(i)))

  def map2[B, C](g: SGen[B])(f: (A, B) => C): SGen[C] = for {
    a <- this
    b <- g
  } yield f(a, b)
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(i => Gen.listOfN(i, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(i =>
    if (i == 0) Gen.listOfN(1, g) else Gen.listOfN(i, g)
  )

  def int: SGen[Int] = Gen(State(RNG.int)).unsized

  def positiveInt: SGen[Int] = int.map(Math.abs)

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type Max = Int

  def forAll[A](name: String, g: Gen[A])(p: A => Boolean): Prop = Prop({
    (max, n, rng) =>
      randomStream(g)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (p(a)) Passed else Falsified(name + ":" + a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find {
        case _: Falsified => true
        case _ => false
      }.getOrElse(Passed)

  })

  def forAll[A](name: String, sg: SGen[A])(p: A => Boolean): Prop =
    forAll(name, sg.forSize)(p)

  def forAll[A](name: String, g: Int => Gen[A])(p: A => Boolean): Prop = Prop({
    (max, n, rng) =>
      val casesPerTest = (n + (max - 1)) / max

      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map { i =>
        forAll(name, g(i))(p)
      }
      val prop: Prop = props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerTest, rng)
      }).toList.reduce(_ && _)

      prop.run(max, n, rng)

  })

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(cg => Some(g.sample.run(cg)))

  def buildMsg[A](v: A, e: Exception): String =
    s"""
       |test case: $v"
       |generated an exception: ${e.getMessage}
       |stack trace:
       |  ${e.getStackTrace.mkString("\n")}
     """.stripMargin


  def run(p: Prop, maxSize: Int = 100, testCases: Int = 10, rng: RNG = RNG.rng): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg.")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }
  }

  def runResult(p: Prop, maxSize: Int = 100, testCases: Int = 10, rng: RNG = RNG.rng): Result =
    p.run(maxSize, testCases, rng)

}

import ch8.propertytesting.Prop._

sealed trait Result

case object Passed extends Result

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result

case class Prop(run: (Max, TestCases, RNG) => Result) {
  def tag(msg: String) = Prop((max, tc, rng) => run(max, tc, rng) match {
    case Falsified(f, sc) => Falsified(f + "\n" + msg, sc)
    case x => x
  })

  def &&(p: Prop): Prop = {
    Prop((max, tc, rng) =>
      run(max, tc, rng) match {
        case Passed => p.run(max, tc, rng)
        case x => x
      })
  }

  def ||(p: Prop): Prop = {
    Prop((max, tc, rng) => run(max, tc, rng) match {
      case Falsified(failure, _) => p.tag(failure).run(max, tc, rng)
      case x => x
    })
  }
}

import ch8.propertytesting.Prop._

class PropTest extends FunSuite with Matchers {

  test("choose") {
    println(Gen.choose(20, 25).sample.run(RNG.rng))
  }
  test("boolean") {
    println(Gen.boolean.sample.run(RNG.rng))
  }
  test("listOfN") {
    println(Gen.listOfN(10, Gen.boolean).sample.run(RNG.rng))
    println(Gen.listOfN(10, Gen.int).sample.run(RNG.rng))
  }
  test("option[Int]") {
    println(Gen.listOfN(10, Gen.option(Gen.int)).sample.run(RNG.rng))
  }
  test("fromOption") {
    println(Gen.listOfN(10, Gen.fromOption(Gen.option(Gen.int))).sample.run(RNG.rng))
  }
  test("string") {
    println(Gen.listOfN(10, Gen.string).sample.run(RNG.rng))
  }
  test("union") {
    println(Gen.listOfN(10, Gen.union(Gen.unit(1), Gen.unit(0))).sample.run(RNG.rng))
  }
  test("sequence") {
    Gen.sequence(List(Gen.unit(0), Gen.unit(1))).sample.run(RNG.rng)._1 should be(List(0, 1))
  }

  test("Gen.list") {
    println(Gen.list(Gen.int).sample.run(RNG.rng))
  }
  test("forAll show") {
    Prop.run(forAll("greater than", Gen.int)(i => i > Integer.MIN_VALUE))
  }
  test("p1 && p2") {
    Prop.run(forAll("eq", SGen.positiveInt)(i => i == Math.abs(i))
      && forAll("gt", SGen.positiveInt)(i => i < 0))
  }
  test("p1 || p2") {
    Prop.run(forAll("eq", SGen.positiveInt)(i => i == Math.abs(i)) || forAll("gt", SGen.positiveInt)(i => i < 0))
  }

  test("sgen list of n") {
    println(SGen.listOf(Gen.unit(1)).forSize(5).sample.run(RNG.rng))
  }

  test("max") {
    Prop.run(forAll("max", SGen.listOf1(Gen.choose(-10, 10))){ ns =>
      val max = ns.max
      !ns.exists(_ > max)
    })
  }

  test("sorted") {
    val intList = SGen.listOf1(Gen.choose(-10, 10))
    Prop.run(forAll("sorted max", intList){ ns =>
      val max = ns.max
      ns.sorted.last == max
    } && forAll("sorted min", intList) { ns =>
      val min = ns.min
      ns.sorted.head == min
    }, 10, 1221)
  }
}