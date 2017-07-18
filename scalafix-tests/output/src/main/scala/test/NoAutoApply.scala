package fix

object NoAutoApply {
  val x: Iterator[Int] = ???
  def foo() = println(1)
  def bar = 32
  foo()
  foo()
  bar
  x.next()
  class buz() {
    def qux() = List(1)
  }
  new buz().qux().toIterator.next()
  println(1.toString())
}
