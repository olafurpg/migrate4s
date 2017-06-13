/* ONLY
rewrite = ExplicitImplicitArgs
 */
package test

import scala.collection.immutable.TreeSet

object ExplicitImplicitArgs {
  TreeSet(1, 2, 3).map(x => x + 1)
  List(1).map(_ + 2)
}
