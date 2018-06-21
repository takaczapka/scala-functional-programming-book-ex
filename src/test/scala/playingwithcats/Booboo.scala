package playingwithcats

import org.scalatest.{FunSuite, Matchers}

class Booboo extends FunSuite with Matchers {

  def len(s: String): Int = s.length

  // Test monoid
  test("monoid") {
    import cats._
    import cats.implicits._

    List("a", "b", "c").combineAll shouldBe "abc"
  }

  // Functor
  test("functor") {
    import cats._
    import cats.implicits._

    Functor[List].map(List(1,2,3))(_ * 2) shouldBe List(2, 4, 6)
  }

  test("fproduct - extending lists") {
    import cats.implicits._

    // fproduct is a function of an Functor, there is an implicit conversion from List to Functor
    List("booboo", "rocky").fproduct(len) shouldBe List(("booboo",6), ("rocky",5))

    List("booboo", "rocky").fproduct(len).toMap shouldBe Map(("booboo",6), ("rocky",5))
  }

  // Apply

  test("apply") {
    import cats._
    import cats.implicits._

    val addOne : Int => Int = _ + 1

    Apply[List].ap(List(addOne))(List(1,2,3)) shouldBe List(2, 3, 4)

    Apply[Option].ap(Some(addOne))(Some(4)) shouldBe Some(5)

    (addOne.some ap 5.some) shouldBe Some(6)

    val addBoth=(a: Int , b: Int) => a + b

    (addBoth.some ap2 (2.some, 4.some)) shouldBe Some(6)


    val append = (a: Int, b: List[Int]) => a :: b

    (append.some ap2 (4.some , List(2,3).some)) shouldBe Some(List(4, 2, 3))
  }

  test("apply with |@|") {
    import cats.implicits._

    (5.some |@| 6.some).map(_ + _) shouldBe Some(11)

    def addOptions( a: Option[Int], b: Option[Int])= (a |@| b).map(_+_)

    addOptions(5.some, 4.some) shouldBe Some(9)
    addOptions(None, 4.some) shouldBe None
  }


  test("Eq") {
//    import cats._
//    import cats.data._
    import cats.implicits._

//    1 === 1  <- doesn't work because it confilics in Scala test triple equals
    1 =!= 2
  }




  // TBC https://medium.com/@abu_nadhr/scala-cats-library-for-dummies-part-3-fd3b185088f0

  test("ASdasd") {
    import cats.implicits._

    val ss: Either[String, List[Int]] = List[Either[String, Int]](Left("a"), Right(2)).sequence[Either[String, ?], Int]
    ss shouldBe Left("a")

  }

  test("sequence") {
    import cats.implicits._
//    import cats.Traverse.ops._

    List(Option(1), Option(2)).sequence[Option, Int] shouldBe Option(List(1, 2))
    List(Option(1), None).sequence[Option, Int] shouldBe None
  }


  test("sequence on list of eithers") {
    import cats.implicits._

    val res1: List[Either[String, Int]] = List(Left("bad"), Left("worse"), Right(1))
    val res2: List[Either[String, Int]] = List(Right(1), Right(2))

    // ? coming from https://github.com/non/kind-projector
    res1.sequence[Either[String, ?], Int] shouldBe Left("bad")

    res2.sequence[Either[String, ?], Int] shouldBe Right(List(1, 2))



  }

  import cats.data._
  import cats.data.Validated._
  import cats.implicits._
  import cats.syntax.either

  test("valid nel") {
    val a: ValidatedNel[Int, String] = "a".validNel
    val b: ValidatedNel[Int, String] = "b".validNel

    List(a, b).sequence shouldBe List("c", "b").valid



  }

  test("what? doesn't work in gradle") {
    val b : List[Either[String, Int]] = List(Right(1), Right(2), Left("dupa"))
    b.sequence

  }
}
