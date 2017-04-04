package scalafix.config

import scala.meta.Position
import scala.meta.internal.inputs._
import scalafix.util.Severity

import java.io.PrintStream

import metaconfig.ConfigReader
import metaconfig.Reader

@ConfigReader
class ScalafixReporter(minSeverity: Severity,
                       outStream: PrintStream,
                       filter: FilterMatcher,
                       includeLoggerName: Boolean) {
  implicit val FilterMatcherReader: Reader[FilterMatcher] = filter.reader
  def withMinSeverity(newMinSeverity: Severity): ScalafixReporter =
    new ScalafixReporter(minSeverity = newMinSeverity,
                         outStream,
                         filter,
                         includeLoggerName)
  private def report(message: String, position: Position, severity: Severity)(
      implicit ctx: LogContext): Unit =
    if (severity >= minSeverity && filter.matches(ctx.enclosing.value)) {
      val enclosing =
        if (includeLoggerName) s"(${ctx.enclosing.value}) " else ""
      outStream.println(
        position.start.formatMessage(enclosing + severity.toString, message))
    }
  def trace(message: String, position: Position = Position.None)(
      implicit ctx: LogContext): Unit =
    report(message, position, Severity.Trace)
  def debug(message: String, position: Position = Position.None)(
      implicit ctx: LogContext): Unit =
    report(message, position, Severity.Debug)
  def info(message: String, position: Position = Position.None)(
      implicit ctx: LogContext): Unit =
    report(message, position, Severity.Info)
  def warn(message: String, position: Position = Position.None)(
      implicit ctx: LogContext): Unit =
    report(message, position, Severity.Warn)
  def error(message: String, position: Position = Position.None)(
      implicit ctx: LogContext): Unit =
    report(message, position, Severity.Error)
}

object ScalafixReporter {
  val default = new ScalafixReporter(
    Severity.Info,
    Console.out,
    FilterMatcher.matchEverything,
    includeLoggerName = false
  )
}
