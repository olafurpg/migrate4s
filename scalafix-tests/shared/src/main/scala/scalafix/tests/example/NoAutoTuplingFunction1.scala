package scalafix.tests.example

object NoAutoTuplingFunction1 {
  val foo = (a: (Int, Boolean)) => a
}
