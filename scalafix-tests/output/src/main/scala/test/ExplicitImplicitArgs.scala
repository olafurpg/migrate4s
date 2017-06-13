package test

import scala.collection.immutable.TreeSet

object ExplicitImplicitArgs {
  TreeSet(1, 2, 3)(scala.math.Ordering.Int).map(x => x + 1)(scala.collection.immutable.SortedSet.newCanBuildFrom[Int](scala.math.Ordering.Int))
  List(1).map(_ + 2)
}