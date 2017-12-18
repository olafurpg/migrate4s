/* ONLY
rule = NoInfer
NoInfer.excludeEnclosing = [
  "sourcecode.Text.generate"
  "scala.collection.immutable.List.apply"
  "scala.collection.immutable.List#`++`"
]
*/
package test

object NoInferExclude {
  def blah(text: sourcecode.Text[Any]): Unit = println(text.source)
  blah(1)
  List(1, "")
  List.apply(1, "")
  sealed trait ADT
  case class A() extends ADT
  case class B() extends ADT
  val x: List[ADT] = List(A()) ++ List(B())
}
