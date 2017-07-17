package ch7.parallelism

import java.util.concurrent._

import org.scalatest.{FunSuite, Matchers}

object Par {

  type Par[A] = ExecutorService => Future[A]

  // it doesn't take an execution service and uses UnitFuture so beware and check asyncF and fork
  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def get(timeout: Long, unit: TimeUnit): A = get

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  // implementation of the future which will respect timeouts of mapped futures
  private case class MappingFuture[A, B, C](fa: Future[A], fb: Future[B], f: (A, B) => C) extends Future[C] {

    private def timeIt[BLOCK](block: => BLOCK): (Long, BLOCK) = {
      val start = System.currentTimeMillis()

      val result = block

      (System.currentTimeMillis() - start, result)
    }

    override def get(): C = {
      f(fa.get, fb.get)
    }

    override def get(timeout: Long, unit: TimeUnit): C = {
      val available = unit.toMillis(timeout)
      val (expiredInFa, a) = timeIt(
        fa.get(available, TimeUnit.MILLISECONDS)
      )

      val left = available - expiredInFa

      if (left <= 0) throw new TimeoutException

      val b = fb.get(left, TimeUnit.MILLISECONDS)

      f(a, b)
    }

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // (es: ExecutorService) => {
  //    val af: Future[A] = a(es)
  //    val bf: Future[B] = b(es)
  //
  //    UnitFuture(f(af.get, bf.get))
    map2WithTimeouts(a, b)(f)

  //  }

  def map2WithTimeouts[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af: Future[A] = a(es)
    val bf: Future[B] = b(es)

    MappingFuture(af, bf, f)
  }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call: A = a(es).get
  })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // very important - it does fork!
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def run[A](s: ExecutorService)(p: Par[A]): Future[A] = p(s)

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((a, _) => a.sorted)

  def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit(()))((a, _) => f(a))

  def sortParWithMap(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def sequence[A](p: List[Par[A]]): Par[List[A]] =
    p.foldLeft(unit(Nil): Par[List[A]])((b, pa) => map2(pa, b)((aa, bb) => aa :: bb))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = sequence(ps.map(asyncF(f)))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val ps: List[Par[List[A]]] = as.map(asyncF(a => if (f(a)) List(a) else List()))

    map(sequence(ps))(_.flatten)
  }

  def sum(ints: List[Int]): Par[Int] = {
    val fi: List[Par[Int]] = ints.map(asyncF(a => a))

    map(sequence(fi))(_.sum)
  }

  def sum2(ints: List[Int]): Par[Int] = {
    if (ints.size <= 1)
      lazyUnit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(sum2(l), sum2(r))(_ + _)
    }
  }

  def max(ints: List[Int]): Par[Int] = {
    val fi: List[Par[Int]] = ints.map(asyncF(a => a))

    map(sequence(fi))(_.max)
  }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = (es: ExecutorService) => {
    val af: Future[A] = a(es)
    val bf: Future[B] = b(es)
    val cf: Future[C] = c(es)

    UnitFuture(f(af.get, bf.get, cf.get))
  }

}


import ch7.parallelism.Par._

class ParTest extends FunSuite with Matchers {

  private val es = Executors.newFixedThreadPool(5)

  private def eval[A](p: Par[A]): A = Par.run(es)(p).get()

  test("sum") {
    eval(sum(List(1, 2, 3, 4, 5, 6, 7, 8, 7, 6, 5, 4, 3, 2, 1))) should be(64)
  }

  test("sum2") {
    eval(sum2(List(1, 2, 3, 4, 5, 6, 7, 8, 7, 6, 5, 4, 3, 2, 1))) should be(64)
  }

  test("max") {
    eval(max(List(1, 2, 3, 4, 5, 6, 7, 8, 7, 6, 5, 4, 3, 2, 1))) should be(8)
  }

  test("sum locking") {

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      p(e).get == p2(e).get

    val es = Executors.newFixedThreadPool(1)

    val a: Par[Int] = lazyUnit(1 + 32)
    equal(es)(a, fork(a))
  }
}
