package scalafix.dotc

import java.nio.file.Path
import java.nio.file.Paths
import scala.collection.mutable
import scala.meta._
import scala.meta.internal.semantic.{schema => s}
import scalafix.rewrite.ScalafixDatabase
import scalafix.syntax._
import dotty.tools.dotc.interfaces.Diagnostic
import org.scalameta.logger

object DottyMirror {
  def fromScalacMirror(scalacMirror: ScalafixDatabase,
                       compiler: DottyCompiler): ScalafixDatabase = {
    val diagnostics = compiler.compile(
      scalacMirror.sources.map(_.input),
      scalacMirror.dependencyClasspath.shallow.map(_.toString()))

    val builder = mutable.HashMap.empty[Path, s.Attributes]
    diagnostics.foreach { d =>
      if (d.position().isPresent) {
        val pos = d.position().get
        val path = Paths.get(pos.source().path())
        val attrs = builder.getOrElseUpdate(
          path,
          s.Attributes(
            dialect = "Dotty",
            filename = compiler.sourceroot.relativize(path).toString
          )
        )

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
    builder.keys.foreach(path => logger.elem(path))
    val sourceRoot = Sourcepath(AbsolutePath(compiler.sourceroot))
    val dottyMirror =
      s.Database.apply(builder.values.toList).toMeta(Some(sourceRoot))
    ScalafixDatabase(dottyMirror, scalacMirror.dependencyClasspath)
  }
}
