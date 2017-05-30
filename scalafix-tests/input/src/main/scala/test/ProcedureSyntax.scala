/*
rewrites = ProcedureSyntax
 */
package test

object ProcedureSyntax {
  // This is a comment
  def main(args: Seq[String]) {
    var number = 2
    def increment(n: Int) {
      number += n
    }
    increment(3)
    args.foreach(println)
  }
  def foo {
    println(1)
  }
  def main() /* unit */ {}
}

