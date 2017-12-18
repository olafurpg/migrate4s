package scalafix.tests.core

import org.scalameta.logger
import scala.meta._
import scala.meta.contrib._
import scala.meta.interactive.InteractiveSemanticdb
import scala.meta.internal.scalafix.ScalafixScalametaHacks
import scalafix.SemanticdbIndex
import scalafix.internal.util.EagerInMemorySemanticdbIndex
import scalafix.testkit.DiffAssertions
import org.scalatest.FunSuite

class DesugarTest extends FunSuite with DiffAssertions {
  val global = InteractiveSemanticdb.newCompiler()
  def check(original: String, expected: String): Unit = {
    test(logger.revealWhitespace(original)) {
      val db = InteractiveSemanticdb.toDocument(global, original)
      val index = EagerInMemorySemanticdbIndex(
        Database(db :: Nil),
        Sourcepath(Nil),
        Classpath(Nil))
      val obtained = index.desugar(db.input.parse[Source].get)
      assertNoDiff(obtained.syntax, expected)
      // TODO(olafur) figure out why this fails
//      obtained.foreach(tree => {
//        withClue(tree) {
//          assert(tree.pos.input != Input.None)
//        }
//      })
    }
  }

  check(
    """
      |object DesugarTest {
      | List(1).map(_ + 1)
      | 'ab + "ba"
      |}
    """.stripMargin,
    source"""
      object DesugarTest {
        List
          .apply[Int](1)
          .map[Int, List[Int]](_ + 1)(
            scala.collection.immutable.List.canBuildFrom[Int])
        scala.Predef.any2stringadd[Symbol]('ab) + "ba"
      }
       """.syntax
  )
}
