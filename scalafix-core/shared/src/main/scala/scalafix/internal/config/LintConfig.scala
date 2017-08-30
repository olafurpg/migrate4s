package scalafix.internal.config

import scalafix.lint.LintSeverity
import metaconfig.ConfDecoder

case class LintConfig(
    reporter: ScalafixReporter = PrintStreamReporter.default,
    explain: Boolean = false,
    ignore: FilterMatcher = FilterMatcher.matchNothing,
    info: FilterMatcher = FilterMatcher.matchNothing,
    warning: FilterMatcher = FilterMatcher.matchNothing,
    error: FilterMatcher = FilterMatcher.matchNothing
) {
  val reader: ConfDecoder[LintConfig] =
    ConfDecoder.instanceF[LintConfig] { c =>
      (
        c.getOrElse("reporter")(reporter) |@|
          c.getOrElse("explain")(explain) |@|
          c.getOrElse("ignore")(ignore) |@|
          c.getOrElse("info")(info) |@|
          c.getOrElse("warning")(warning) |@|
          c.getOrElse("error")(error)
      ).map { case (((((a, b), c), d), e), f) => LintConfig(a, b, c, d, e, f) }
    }

  def getConfiguredSeverity(key: String): Option[LintSeverity] =
    Option(key).collect {
      case error() => LintSeverity.Error
      case warning() => LintSeverity.Warning
      case info() => LintSeverity.Info
    }
}

object LintConfig {
  lazy val default = LintConfig()
}
