/* ONLY
rule = NoInfer
NoInfer.excludeEnclosing = [
  "sourcecode.Text.generate"
  "scala.collection.immutable.List.apply"
]
*/
package test

object NoInferExclude {
  def blah(text: sourcecode.Text[Any]): Unit = println(text.source)
  blah(1)
  List(1, "")
  List.apply(1, "")
}
