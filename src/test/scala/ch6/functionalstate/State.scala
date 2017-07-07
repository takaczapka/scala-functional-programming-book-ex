package ch6.functionalstate

import org.scalatest.{FunSuite, Matchers}


case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s2) = run(s)
    (f(a), s2)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = for {
    a <- this
    b <- sb
  } yield f(a, b)
}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State(s => {
    fs.foldRight((Nil: List[A], s)) { case (state, (res, current)) =>
      val (a, nxtSt): (A, S) = state.run(current)
      (a :: res, nxtSt)
    }
  })

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}


sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

trait MachineState extends State[Machine, (Int, Int)] {
  def process(i: Input): MachineState
}

object CandyMachine {

  def process(m: Machine)(i: Input): Machine =
    i match {
      case Coin =>
        m.copy(locked = false, coins = m.coins + 1)
      case Turn =>
        if (m.candies > 0 && !m.locked) m.copy(locked = true, candies = m.candies - 1)
        else m
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(m => {

    val r = inputs.foldLeft(m) { case (currentMachine, input) => process(currentMachine)(input) }

    ((r.candies, r.coins), r)
  })
}

import CandyMachine._
class SimTest extends FunSuite with Matchers {
  test("sim") {
    val m = Machine(locked = true, 5, 10)
    val seq = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)

    val ((candies, coins), _) : ((Int, Int), Machine) = simulateMachine(seq).run(m)

    (candies, coins) should be ((1, 14))
  }
}
