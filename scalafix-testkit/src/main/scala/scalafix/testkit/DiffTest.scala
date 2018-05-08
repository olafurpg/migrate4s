package scalafix.testkit

import org.langmeta.internal.io.FileIO
import org.langmeta.internal.io.PathIO
import scala.meta._
import scalafix.SemanticdbIndex
import scalafix.Rule
import scalafix.internal.config.LazySemanticdbIndex
import scalafix.internal.config.ScalafixConfig
import scalafix.reflect.ScalafixReflect
import org.scalatest.exceptions.TestFailedException
import scalafix.internal.reflect.ClasspathOps
import scalafix.internal.rule.DocOps._
import scalafix.v1.SemanticDoc
import org.langmeta.internal.ScalafixLangmetaHacks._

case class DiffTest(
    filename: RelativePath,
    doc: SemanticDoc,
    config: () => (Rule, ScalafixConfig),
    isSkip: Boolean,
    isOnly: Boolean) {

  @deprecated("Use doc instead", "0.6.0")
  def document: SemanticDoc = doc

  @deprecated("Use doc.input instead", "0.6.0")
  def input: Input = doc.input

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
        val doc = SemanticDoc.fromPath(
          relpath,
          classpath,
          symtab,
          ScalafixConfig.DefaultDialect)
        val config: () => (Rule, ScalafixConfig) = { () =>
          doc.input.tokenize.get
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
        }
        DiffTest(
          filename = relpath,
          doc = doc,
          config = config,
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
