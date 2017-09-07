package scalafix
package internal.config

import java.io.PrintStream
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.parsers.Parse
import metaconfig._

case class ScalafixConfig(
    parser: Parse[_ <: Tree] = Parse.parseSource,
    debug: DebugConfig = DebugConfig(),
    groupImportsByPrefix: Boolean = true,
    fatalWarnings: Boolean = true,
    reporter: ScalafixReporter = ScalafixReporter.default,
    patches: ConfigRulePatches = ConfigRulePatches.default,
    dialect: Dialect = Scala211,
    lint: LintConfig = LintConfig.default,
    encoding: Charset = StandardCharsets.UTF_8
) {

  def withFreshReporters: ScalafixConfig = copy(
    reporter = reporter.reset,
    lint = lint.copy(
      reporter = lint.reporter.reset
    )
  )

  val reader: ConfDecoder[ScalafixConfig] =
    ConfDecoder.instanceF[ScalafixConfig] { conf =>
      import conf._
      (
        getOrElse("fatalWarnings")(fatalWarnings) |@|
          getOrElse("reporter")(reporter) |@|
          getOrElse("patches")(patches)(patches.reader) |@|
          getOrElse("dialect")(dialect) |@|
          getOrElse("lint")(lint)(lint.reader) |@|
          getOrElse("encoding")(encoding)
      ).map {
        case (((((a, b), c), d), e), f) =>
          copy(
            fatalWarnings = a,
            reporter = b,
            patches = c,
            dialect = d,
            lint = e,
            encoding = f
          )
      }

    }

  def withOut(out: PrintStream): ScalafixConfig = copy(
    reporter = reporter match {
      case r: PrintStreamReporter => r.copy(outStream = out)
      case _ => ScalafixReporter.default.copy(outStream = out)
    }
  )
}

object ScalafixConfig {

  lazy val default = ScalafixConfig()
  implicit lazy val ScalafixConfigDecoder: ConfDecoder[ScalafixConfig] =
    default.reader

  /** Returns config from current working directory, if .scalafix.conf exists. */
  def auto(workingDirectory: AbsolutePath): Option[Input] = {
    val file = workingDirectory.resolve(".scalafix.conf")
    if (file.isFile && file.toFile.exists())
      Some(Input.File(file))
    else None
  }

  def fromInput(
      input: Input,
      index: LazySemanticdbIndex,
      extraRules: List[String] = Nil)(
      implicit decoder: ConfDecoder[Rule]
  ): Configured[(Rule, ScalafixConfig)] =
    configFromInput(input, index, extraRules)

}
