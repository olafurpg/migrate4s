package scalafix.dotc

import java.nio.file.Path
import java.nio.file.Paths
import scala.collection.mutable
import scala.meta.Mirror
import scala.meta.internal.semantic.{schema => s}
import scala.meta.semantic.Database
import scalafix.rewrite.ScalafixDatabase
import dotty.tools.dotc.interfaces.Diagnostic
import org.scalameta.logger
import scalafix.syntax._

object DottyMirror {
  def fromMirror(mirror: ScalafixDatabase, compiler: DottyCompiler): Mirror = {
    val dottyMirror = {
      val diagnostics = compiler.compile(
        mirror.sources.map(_.input),
        mirror.dependencyClasspath.shallow.map(_.toString()))
      logger.elem(diagnostics.toList)
      val builder = mutable.HashMap.empty[Path, s.Attributes]

      diagnostics.foreach { d =>
        if (d.position().isPresent) {
          val pos = d.position().get
          val path = Paths.get(pos.source().path())
          val attrs =
            builder.getOrElseUpdate(path,
                                    s.Attributes(filename = path.toString))
          val severity = d.level() match {
            case Diagnostic.ERROR => s.Message.Severity.ERROR
            case Diagnostic.WARNING => s.Message.Severity.WARNING
            case Diagnostic.INFO => s.Message.Severity.INFO
          }
          val range = s.Range(start = pos.start(), end = pos.end())
          val message = s.Message(Some(range), severity, d.message())
          builder(path) = attrs.copy(messages = message +: attrs.messages)
        }
      }
      Database(builder.values.toSeq)
    }
  }
  def fromMessages(diagnostics: List[Diagnostic]): Mirror = {
    ???
  }
}
