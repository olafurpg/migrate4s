/*
rules = [
  Disable
]

Disable.unlessInside = [
  {
    safeBlock = "test.DisableUnlessInside.IO"
    symbols = [
      {
        symbol = "scala.Predef.println"
        message = "println has side-effects"
      }
      "java.lang.System.currentTimeMillis"
    ]
  }
  {
    safeBlocks = [ "scala.Option", "scala.util.Try" ]
    symbols = [
      {
        symbol = "test.DisableUnlessInside.dangerousFunction"
        message = "the function may return null"
      }
    ]
  }
]
*/
package test

object DisableUnlessInside {
  object IO { // IO we deserve
    def apply[T](run: => T): Nothing = ???
  }

  Option(IO(println("boo!"))) // ok
}
