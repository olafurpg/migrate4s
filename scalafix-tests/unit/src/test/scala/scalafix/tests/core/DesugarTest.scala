package scalafix.tests.core

import scala.meta._
import scala.meta.interactive.InteractiveSemanticdb
import scala.tools.nsc.interactive.Global
import scalafix.internal.util.EagerInMemorySemanticdbIndex
import scalafix.testkit.DiffAssertions
import org.scalameta.logger
import org.scalatest.FunSuite

class DesugarTest extends FunSuite with DiffAssertions {

  val global: Global = InteractiveSemanticdb.newCompiler()

  def check(original: String, expected: String): Unit = {
    test(logger.revealWhitespace(original)) {
      val db = InteractiveSemanticdb.toDocument(global, original)
      val index = EagerInMemorySemanticdbIndex(
        Database(db :: Nil),
        Sourcepath(Nil),
        Classpath(Nil))
      val obtained = index.desugar(db.input.parse[Source].get)
      assertNoDiff(obtained.syntax, expected)
    }
  }

  check(
    """
      |object Apply {
      |  List(1).map(_ + 1)
      |}
    """.stripMargin,
    source"""
      object Apply {
        List
          .apply[Int](1)
          .map[Int, List[Int]](_ + 1)(
            scala.collection.immutable.List.canBuildFrom[Int])
      }
       """.syntax
  )

  check(
    """
      |object Conversion {
      |   locally {
      |     'ab + "ba"
      |   }
      |}
    """.stripMargin,
    source"""
      object Conversion {
        locally[String] {
          scala.Predef.any2stringadd[Symbol]('ab) + "ba"
        }
      }
       """.syntax
  )

  check(
    """
      |object Def {
      |  def blah(implicit t: sourcecode.Text[Any]) = t.source
      |  blah(1)
      |}
    """.stripMargin,
    source"""
      object Def {
        def blah(implicit t: sourcecode.Text[Any]): String = t.source
        blah(sourcecode.Text.generate[Any](1))
      }
       """.syntax
  )

  check(
    """
      |object For {
      |  for (i <- 1.to(10); j <- 2.to(20)) yield i + j
      |}
    """.stripMargin,
    source"""
      object For {
        for (i <- scala.Predef.intWrapper(1).to(10);
             j <- scala.Predef.intWrapper(2).to(20))
          yield i + j
      }
       """.syntax
  )

  check(
    """
      |object Infix {
      |  sealed trait ADT
      |  case class A() extends ADT
      |  case class B() extends ADT
      |  val x: List[ADT] = List(A()) ++ List(B())
      |}
    """.stripMargin,
    source"""
       object Infix {
         sealed trait ADT
         case class A() extends ADT
         case class B() extends ADT
         val x: List[ADT] = (List.apply[A](A.apply()) ++ List.apply[B](B.apply()))(scala.collection.immutable.List.canBuildFrom[Product with Serializable with ADT {}])
       }
        """.syntax
  )
}
