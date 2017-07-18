/*
rewrite = ExpandSugars
 */
package fix

object ExpandSugars {
  val x = List(1).map(_ + 1)
  x + "foobar"
}
