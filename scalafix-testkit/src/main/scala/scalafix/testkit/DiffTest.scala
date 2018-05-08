package scalafix.testkit

import org.langmeta.internal.ScalafixLangmetaHacks._
import org.langmeta.internal.io.FileIO
import org.langmeta.internal.io.PathIO
import org.scalatest.exceptions.TestFailedException
import scala.meta._
import scalafix.Rule
import scalafix.SemanticdbIndex
import scalafix.internal.config.ScalafixConfig
import scalafix.internal.reflect.ClasspathOps
import scalafix.reflect.ScalafixReflect
import scalafix.v1.SemanticDoc

case class TestInputs(
    rule: Rule,
    config: ScalafixConfig,
    doc: SemanticDoc
)

case class DiffTest(
    filename: RelativePath,
    inputs: () => TestInputs,
    isSkip: Boolean,
    isOnly: Boolean) {

  @deprecated("Use inputs().doc instead", "0.6.0")
  def document: SemanticDoc = inputs().doc

  @deprecated("Use inputs().doc.input instead", "0.6.0")
  def input: Input = inputs().doc.input

  def name: String = filename.toString()
}

object DiffTest {

  private val PrefixRegex = "\\s+(ONLY|SKIP)".r
  private def stripPrefix(str: String) = PrefixRegex.replaceFirstIn(str, "")

  def assertNotONLY(comment: Token.Comment): Unit = {
    if (comment.text.contains("ONLY")) {
      throw new TestFailedException(
        formatMessage(
          comment.pos,
          "error",
          """ONLY is no longer supported, use `testOnly path.to.Suitew -- -z "suiteName"` instead"""
        ),
        1
      )
    }
  }

  def fromSourceDirectories(
      sourceDirectories: List[AbsolutePath],
      classpath: Classpath): Seq[DiffTest] = {
    val symtab = ClasspathOps.newSymbolTable(classpath).getOrElse {
      throw new IllegalArgumentException(
        s"Unable to load symbol table for classpath $classpath")
    }
    sourceDirectories.flatMap { dir =>
      for {
        relpath <- FileIO.listAllFilesRecursively(dir).files
        if PathIO.extension(relpath.toNIO) == "scala"
      } yield {
        val inputs: () => TestInputs = { () =>
          val doc = SemanticDoc.fromPath(
            relpath,
            classpath,
            symtab,
            ScalafixConfig.DefaultDialect)
          val (rule, config) = doc.input.tokenize.get
            .collectFirst {
              case comment: Token.Comment =>
                assertNotONLY(comment)
                val input = Input.Slice(
                  doc.input,
                  comment.pos.start + 2,
                  comment.pos.end - 2)
                ScalafixConfig.fromInput(input)(ScalafixReflect.syntactic).get
            }
            .getOrElse(throw new TestFailedException(
              s"Missing scalafix configuration inside comment at top of file $relpath",
              0))
          TestInputs(rule, config, doc)
        }
        DiffTest(
          filename = relpath,
          inputs = inputs,
          isSkip = false,
          isOnly = false
        )
      }
    }
  }

  @deprecated("Use fromSourceDirectories instead", "0.6.0")
  def fromSemanticdbIndex(index: SemanticdbIndex): Seq[DiffTest] = Nil

  def testToRun(tests: Seq[DiffTest]): Seq[DiffTest] = {
    val onlyOne = tests.exists(_.isOnly)
    def testShouldRun(t: DiffTest): Boolean = !onlyOne || t.isOnly
    tests.filter(testShouldRun)
  }

}
