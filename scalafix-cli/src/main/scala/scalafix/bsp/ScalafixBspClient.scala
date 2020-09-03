package scalafix.bsp

import ch.epfl.scala.bsp4j.BuildClient
import ch.epfl.scala.bsp4j.ShowMessageParams
import ch.epfl.scala.bsp4j.LogMessageParams
import ch.epfl.scala.bsp4j.DidChangeBuildTarget
import ch.epfl.scala.bsp4j.TaskFinishParams
import ch.epfl.scala.bsp4j.TaskStartParams
import ch.epfl.scala.bsp4j.PublishDiagnosticsParams
import ch.epfl.scala.bsp4j.TaskProgressParams
import java.io.PrintStream
import java.nio.file.Paths
import java.net.URI
import java.{util => ju}
import java.util.concurrent.ConcurrentHashMap
import ch.epfl.scala.bsp4j.TaskId
import scala.meta.inputs.Input
import scalafix.internal.util.PositionSyntax._
import scala.meta.inputs.Position

class ScalafixBspClient(out: PrintStream) extends BuildClient {
  private val nops = ju.Collections.newSetFromMap(
    new ConcurrentHashMap[TaskId, java.lang.Boolean]
  )
  override def onBuildShowMessage(params: ShowMessageParams): Unit =
    out.println(
      s"${params.getType().name().toLowerCase()} ${params.getMessage()}"
    )
  override def onBuildLogMessage(params: LogMessageParams): Unit =
    out.println(
      s"${params.getType().name().toLowerCase()} ${params.getMessage()}"
    )
  override def onBuildTaskStart(params: TaskStartParams): Unit = {
    if (params.getMessage().startsWith("Start no-op")) {
      nops.add(params.getTaskId())
    } else {
      out.println(params.getMessage())
    }
  }
  override def onBuildTaskProgress(params: TaskProgressParams): Unit = ()
  override def onBuildTaskFinish(params: TaskFinishParams): Unit = {
    if (!nops.contains(params.getTaskId())) {
      out.println(params.getMessage())
    }
  }
  override def onBuildPublishDiagnostics(
      params: PublishDiagnosticsParams
  ): Unit = {
    val path = Paths.get(URI.create(params.getTextDocument().getUri()))
    val input = Input.File(path)
    params.getDiagnostics().forEach { d =>
      val pos = Position.Range(
        input,
        d.getRange().getStart().getLine(),
        d.getRange().getStart().getCharacter(),
        d.getRange().getEnd().getLine(),
        d.getRange().getEnd().getCharacter()
      )
      val line = d.getRange().getStart().getLine() + 1
      out.println(
        pos.formatMessage(d.getSeverity().name().toLowerCase(), d.getMessage())
      )
    }
  }
  override def onBuildTargetDidChange(params: DidChangeBuildTarget): Unit = ()
}
