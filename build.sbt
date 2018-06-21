name := "functional-programming-in-scala-book-excercises"

version := "1.0"

scalaVersion := "2.12.4"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")

//scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.1.0"

libraryDependencies += "org.typelevel" %% "cats-effect" % "1.0.0-RC2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

