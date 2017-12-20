package scalafix.tests.core

import scala.meta._
import scala.meta.interactive.InteractiveSemanticdb
import scala.tools.nsc.interactive.Global
import scalafix.internal.util.EagerInMemorySemanticdbIndex
import scalafix.testkit.DiffAssertions
import org.scalameta.logger
import org.scalatest.FunSuite

class BaseDesugarTest extends FunSuite with DiffAssertions {

  val global: Global = InteractiveSemanticdb.newCompiler()

  def check(original: String, expected: String): Unit = {
    test(logger.revealWhitespace(original)) {
      val db = InteractiveSemanticdb.toDocument(global, original)
      val index = EagerInMemorySemanticdbIndex(
        Database(db :: Nil),
        Sourcepath(Nil),
        Classpath(Nil))
      val obtained = index.desugar(db.input.parse[Source].get)
      if (obtained.structure != expected.parse[Source].get.structure) {
        assertNoDiff(obtained.syntax, expected)
      }
    }
  }
}

class DesugarTest extends BaseDesugarTest {

  check(
    """
      |object Apply {
      |  List(1).map(_ + List(1).head)
      |}
    """.stripMargin,
    source"""
      object Apply {
        List
          .apply[Int](1)
          .map[Int, List[Int]](_ + List.apply[Int](1).head)(
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
//
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
         val x: List[ADT] = (
         List.apply[A](A.apply()) ++[Product with Serializable with ADT {}, List[ADT]]
           List.apply[B](B.apply()))(
             scala.collection.immutable.List
               .canBuildFrom[Product with Serializable with ADT {}])
       }
        """.syntax
  )

  check(
    """
      |object Unapply {
      |  null match {
      |    case List(1, 2) =>
      |  }
      |}
    """.stripMargin,
    source"""
            object Unapply {
              null match {
                case List(1, 2) =>
              }
            }
        """.syntax
  )

  // infix type positions are buggy
  check(
    """
      |object Infix2 { "str" :: Nil }
    """.stripMargin,
    source"""
       object Infix2 { "str" :: Nil }
       """.syntax
  )
  // ditto: named parameters also affected
  check(
    """
      |object Infix2 {
      |  def foo(lst: List[String]) = lst
      |  foo(lst = "str" :: Nil)
      |}
    """.stripMargin,
    source"""
       object Infix2 { "str" :: Nil }
       """.syntax
  )
}
