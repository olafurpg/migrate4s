package scalafix.testkit

import scala.collection.immutable.Seq
import scala.meta._
import scalafix.Rewrite
import scalafix.config.ScalafixConfig
import scalafix.reflect.ScalafixCompilerDecoder
import scalafix.rewrite.ScalafixDatabase
import org.scalatest.exceptions.TestFailedException

case class DiffTest(filename: RelativePath,
                    original: Input,
                    attributes: Attributes,
                    config: () => (Rewrite, ScalafixConfig),
                    isSkip: Boolean,
                    isOnly: Boolean) {
  def name: String = filename.toString()
  private def prefix =
    if (isOnly) "ONLY "
    else if (isSkip) "SKIP "
    else ""
  def originalStr = new String(original.chars)
}

object DiffTest {

  private val PrefixRegex = "\\s+(ONLY|SKIP)".r
  private def stripPrefix(str: String) = PrefixRegex.replaceFirstIn(str, "")

  def fromMirror(mirror: ScalafixDatabase): Seq[DiffTest] =
    mirror.entries.collect {
      case (input @ Input.LabeledString(label, code), attributes) =>
        val relpath = RelativePath(label)
        val config: () => (Rewrite, ScalafixConfig) = { () =>
          input.tokenize.get
            .collectFirst {
              case Token.Comment(comment) =>
                val decoder =
                  ScalafixCompilerDecoder
                    .fromMirrorOption(Some(mirror))
                ScalafixConfig
                  .fromInput(Input.LabeledString(label, stripPrefix(comment)),
                             Some(mirror))(decoder)
                  .get
            }
            .getOrElse(
              SemanticRewriteSuite.failMissingCommentAtTopOfFile(relpath))
        }
        DiffTest(
          filename = relpath,
          original = input,
          attributes = attributes,
          config = config,
          isSkip = code.contains("SKIP"),
          isOnly = code.contains("ONLY")
        )
    }

  def testToRun(tests: Seq[DiffTest]): Seq[DiffTest] = {
    val onlyOne = tests.exists(_.isOnly)
    def testShouldRun(t: DiffTest): Boolean = !onlyOne || t.isOnly
    tests.filter(testShouldRun)
  }

}
