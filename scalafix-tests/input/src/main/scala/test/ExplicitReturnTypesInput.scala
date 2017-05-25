/*
rewrites = ExplicitReturnTypes
*/
package test

object ExplicitReturnTypesInput {
  implicit val x: Int = 2
  implicit val y: List[Int] = List(2)
  implicit val z: Map[Int, Int] = Map(1 -> 2)
}
