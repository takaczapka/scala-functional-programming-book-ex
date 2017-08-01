package ch10.monoid

import ch8.propertytesting.Gen
import org.scalatest.{FunSuite, Matchers}

sealed trait WC

case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {
  val wcMonoid = new Monoid[WC] {
    override def zero: WC = Part("", 0, "")

    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (s: Stub, p: Part) =>
        Part(s + p.lStub, p.words, p.rStub)
      case (p: Part, s: Stub) =>
        Part(p.lStub, p.words, p.rStub + s)
      case (p1: Part, p2: Part) =>
        Part(p1.lStub, p1.words + p2.words + (if ((p1.rStub + p2.lStub).length > 0) 1 else 0), p2.rStub)
    }
  }

  def wc(s: String): Int =
    Monoid.foldMapV(s, wcMonoid)(c => if (c.isWhitespace) Part("", 0, "") else Stub(String.valueOf(c))) match {
      case Stub(st) => st.length min 1
      case Part(l, w, r) => (l.length min 1) + w + (r.length min 1)
    }
}

import WC._

class WCTest extends FunSuite with Matchers {
  val wcGen: Gen[WC] = {
    val gS : Gen[WC] = Gen.string.map(Stub)
    val gP : Gen[WC] = for {
      stubs <- Gen.listOfN(2, Gen.string)
      words <- Gen.positiveInt.map(_ % 10)
    } yield Part(stubs(0), words, stubs(1))

    Gen.union(gS, gP)
  }

  test("wcMonoid") {
    Monoid.monoidLaws(WC.wcMonoid, wcGen)
  }

  test("wc count") {
    wc("kaya ma kota") shouldBe 3
    wc("") shouldBe 0
  }
}
