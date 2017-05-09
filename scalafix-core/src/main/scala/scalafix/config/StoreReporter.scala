package scalafix.config

import scala.collection.mutable
import scala.meta.inputs.Position
import scalafix.util.Severity

import org.scalameta.logger

case class Message(msg: String, position: Position, severity: Severity)
case class StoreReporter(reporter: ScalafixReporter) extends ScalafixReporter {

  val store = mutable.LinkedHashSet.empty[Message]

  /** Messages with severity < minSeverity are skipped. */
  override def minSeverity: Severity = reporter.minSeverity

  /** Messages whose enclosing scope don't match filter.matches are skipped. */
  override def filter: FilterMatcher = reporter.filter

  /** Present the message to the user.
    *
    * In a command-line interface, this might mean "print message to console".
    * In an IDE, this might mean putting red/yellow squiggly marks under code.
    */
  override def report(message: String, position: Position, severity: Severity)(
      implicit ctx: LogContext): Unit = {
    logger.elem(ctx)
    store += Message(message, position, severity)
    reporter.report(message, position, severity)
  }
}
