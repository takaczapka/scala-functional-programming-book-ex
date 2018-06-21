package playingwithcats


import cats.effect.IO

/*
f(e, e)
// isn't really equivalent to!
val x = e
f(x, x)



[IO Monad] This is kind of a clever trick. It allows Haskell to simultaneously be pure and still have excellent support for manipulating effects
and interacting with the “real world”. But why is it relevant to Scala? After all, Scala is an impure language. We don’t need to go through this
complex rigmarole of describing our effects and composing those descriptions; the language lets us just do it! So why wouldn’t we just, you know,
evaluate the effects that we need evaluated?

The answer is that WE WANT TO REASON ABOUT WHERE AND WHEN OUR EFFECTS ARE EVALUATED. And of course, we want to be able to leverage laws
and abstractions which assume equational semantics for expressions (i.e. referential transparency). Cats is full of these sorts of abstractions,
and cats-laws provides a vast set of laws which describe them. But all of these abstractions and all of these laws break down the moment you introduce
some sort of side-effecting expression. Because, much like our referential transparency example from earlier, these abstractions assume that you can
substitute expressions with their evaluated results, and that’s just not true in the presence of side-effects.

What we need is a data type which allows us to encapsulate Scala-style side-effects in the form of a pure value, on which referential transparency
holds and which we can compose using other well-defined abstractions, such as Monad. Scalaz defines two such data types which meet these criteria:
scalaz.effect.IO and scalaz.concurrent.Task. But in practice, nearly everyone uses Task instead of IO because of its support for asynchronous effects.



The cats-effect project aims to change all of that. The goal of cats-effect is to provide an “easy default” IO type for the cats ecosystem, deeply integrated with cats-core, with all of the features and performance that are required for real world production use. Additionally, cats-effect defines a set of abstractions in the form of several typeclasses which describe what it means to be a pure effect type.

```
import cats.effect.IO

val program = for {
  _ <- IO { println("Welcome to Scala!  What's your name?") }
  name <- IO { Console.readLine }
  _ <- IO { println(s"Well hello, $name!") }
} yield ()
```

We could have just as easily written this program in the following way:

val program = IO {
  println("Welcome to Scala!  What's your name?")
  val name = Console.readLine
  println(s"Well hello, $name!")
}

(!!!) But this gives us less flexibility for composition. Remember that even though program is a pure and referentially transparent value, its definition is not,
which is to say that IO { expr } is not the same as val x = expr; IO { x }. Anything inside the IO { … } block is not referentially transparent, and so should
be treated with extreme care and suspicion. The less of our program we have inside these blocks, the better!

(!!!) IO cannot eagerly evaluate its effects, and similarly cannot memoize its results! If IO were to eagerly evaluate or to memoize,
then we could no longer replace references to the expression with the expression itself, since that would result in a different IO instance to be evaluated separately.


If program does not evaluate eagerly, then clearly there must be some mechanism for asking it to evaluate. After all, Scala is not like Haskell:
we don’t return a value of type IO[Unit] from our main function. IO provides an FFI of sorts for wrapping side-effecting code into pure IO values,
so it must also provide an FFI for going in the opposite direction: taking a pure IO value and evaluating its constituent actions as side-effects.

program.unsafeRunSync()    // uh oh!




>>>>> async

cats.effect.IO provides an additional constructor, async, which allows the construction of IO instances from callback-driven APIs.
This is generally referred to as “asynchronous” control flow, as opposed to “synchronous” control flow (represented by the apply constructor)



http://typelevel.org/blog/2017/05/02/io-monad-for-cats.html
https://blog.jle.im/entry/io-monad-considered-harmful


 */
object Program extends App {

  val program = for {
    _ <- IO {
      println("Welcome to Scala!  What's your name?")
    }
    name <- IO {
      scala.io.StdIn.readLine
    }
    _ <- IO {
      println(s"Well hello, $name!")
    }
  } yield ()

  program.unsafeRunSync()
}


object Progra2 extends App {
  println("Welcome to Scala!  What's your name?")
  val name = scala.io.StdIn.readLine
  println(s"Well hello, $name!")
}


object Program5 extends App {

  val program = for {
//    _ <- IO {
//      println("Welcome to Scala!  What's your name?")
//    }
//    name <- IO {
//      scala.io.StdIn.readLine
//    }
    _ <- IO {
      throw new RuntimeException("asd")
    }
  } yield ()

//  program.unsafeRunSync()
  program.unsafeRunAsync{
    case Left(t) => println("booom: " + t.toString)
    case u => println("done")
  }
}

class IoMonadFun {

}
