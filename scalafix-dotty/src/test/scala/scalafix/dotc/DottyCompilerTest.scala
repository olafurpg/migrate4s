package scalafix.dotc

import scala.meta.inputs.Input
import org.scalameta.logger
import org.scalatest.FunSuite

class DottyCompilerTest extends FunSuite {
  def compileOK(code: String, ok: Boolean = true): Unit = {
    val name = logger.revealWhitespace(code)
    test(name) {
      val compiler = DottyCompiler().get
      val errors = compiler.compile(Input.LabeledString(name, code))
      if (ok) assert(errors.isEmpty)
      else {
        assert(errors.nonEmpty)
        assert(errors.exists(_.position().isPresent))
      }
    }
  }
  def compileFail(code: String): Unit = compileOK(code, ok = false)
  test("DottyCompiler().get") { DottyCompiler().get }
  compileOK(
    """|object HelloWorld {
       |  val x: Int | String = "Hello world!"
       |}""".stripMargin
  )
  compileFail(
    """|object HelloWorld {
       |  val x: Int = "Hello world!"
       |}""".stripMargin
  )
  compileFail(
    """|/* ONLY
       |rewrite = ExplicitImplicitArgs
       | */
       |package test
       |
       |import scala.collection.immutable.TreeSet
       |
       |object ExplicitImplicitArgs {
       |  TreeSet(1, 2, 3).map(x => x + 1)
       |  List(1).map(_ + 2)
       |}
       |""".stripMargin
  )
}
