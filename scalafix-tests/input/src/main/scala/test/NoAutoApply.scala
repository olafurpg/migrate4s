/* ONLY
rewrite = NoAutoApply
 */
package fix

object NoAutoApply {
  object buz {
    def empty[T]() = List.empty[T]
  }
  val x: Iterator[Int] = ???
  def foo() = println(1)
  def bar = 32
  foo
  println(foo)
  foo()
  bar
  x.next
  class buz() {
    def qux() = List(1)
  }
  new buz().qux.toIterator.next
  new java.util.ArrayList[Int]().toString
  val builder = List.newBuilder[Int]
  builder.result()
  builder result ()
  builder.result
  fix.NoAutoApply.buz.empty[String]
  var opt: Option[() => Int] = None
  opt = None
  println(1.toString)
  List(builder) map (_.result)
  builder.##
  (null: scala.reflect.ClassTag[Int]).toString
  def lzy(f: => Int) = {
    var k = f _
    k = () => 3
  }
}
