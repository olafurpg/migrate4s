package fix

object ExpandSugars {
  val x = List.apply[Int](1).map[Int, List[Int]](_ + 1)(scala.collection.immutable.List.canBuildFrom[Int])
  scala.Predef.any2stringadd[List[Int]](x) + "foobar"
}
