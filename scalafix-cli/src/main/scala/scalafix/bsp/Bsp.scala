package scalafix.bsp

import scala.collection.JavaConverters._

import java.nio.file.Path
import java.nio.file.Files
import java.nio.charset.StandardCharsets
import com.google.gson.JsonParser
import org.eclipse.lsp4j.jsonrpc.Launcher
import java.nio.file.StandardOpenOption
import java.io.PrintWriter
import ch.epfl.scala.bsp4j.InitializeBuildParams
import buildinfo.RulesBuildInfo
import ch.epfl.scala.bsp4j.BuildClientCapabilities
import ch.epfl.scala.bsp4j.InitializeBuildResult
import ch.epfl.scala.bsp4j.BuildTarget
import ch.epfl.scala.bsp4j.BuildTargetIdentifier
import ch.epfl.scala.bsp4j.SourcesParams
import java.nio.file.Paths
import java.net.URI
import scala.collection.mutable
import scala.meta.io.Classpath
import scala.meta.io.AbsolutePath
import ch.epfl.scala.bsp4j.CompileParams
import ch.epfl.scala.bsp4j.StatusCode
import ch.epfl.scala.bsp4j.ScalacOptionsParams
import java.io.PrintStream
import scala.meta.internal.io.FileIO
import java.nio.file.FileSystems

class Bsp(
    workspace: Path,
    files: List[Path],
    proc: Process,
    server: ScalafixBspServer
) extends AutoCloseable {
  var initializeResponse: InitializeBuildResult = _
  def initialize(): Unit = {
    initializeResponse = server
      .buildInitialize(
        new InitializeBuildParams(
          "Scalafix",
          RulesBuildInfo.version,
          RulesBuildInfo.bspVersion,
          workspace.toUri().toString(),
          new BuildClientCapabilities(List("scala").asJava)
        )
      )
      .get()
    server.onBuildInitialized()
  }
  lazy val targets: List[BuildTarget] = {
    server.workspaceBuildTargets().get.getTargets().asScala.toList
  }
  lazy val targetIds: List[BuildTargetIdentifier] = {
    targets.map(_.getId())
  }
  lazy val inverseSources: Map[Path, BuildTargetIdentifier] = {
    val sources = server
      .buildTargetSources(new SourcesParams(targetIds.asJava))
      .get()
      .getItems()
    val result = mutable.Map.empty[Path, BuildTargetIdentifier]
    sources.forEach { item =>
      item.getSources().forEach { source =>
        result(source.getUri().toPath) = item.getTarget()
      }
    }
    result.toMap
  }
  private implicit class XtensionUri(uri: String) {
    def toPath: Path = Paths.get(URI.create(uri))
  }
  lazy val scalacOptionsResult = {
    val ids = files
      .flatMap(file => {
        if (Files.isRegularFile(file)) inverseSources.get(file).toList
        else if (Files.isDirectory(file))
          inverseSources.keysIterator
            .filter(_.startsWith(file))
            .flatMap(inverseSources.get)
        else Nil
      })
      .distinct
    val result = server.buildTargetCompile(new CompileParams(ids.asJava)).get()
    require(result.getStatusCode() != StatusCode.ERROR, "compile error")
    Thread.sleep(1000)
    server
      .buildTargetScalacOptions(new ScalacOptionsParams(targetIds.asJava))
      .get()
  }
  def scalacOptions: List[String] =
    scalacOptionsResult
      .getItems()
      .asScala
      .flatMap(_.getOptions().asScala)
      .toList

  val jarPattern = FileSystems.getDefault().getPathMatcher("glob:**.jar")

  lazy val classpaths: Classpath = {
    val buf = mutable.LinkedHashSet.empty[AbsolutePath]
    scalacOptionsResult.getItems().forEach { item =>
      item.getClasspath().forEach { cp =>
        val path = AbsolutePath(cp.toPath)
        val isIgnored = path.isFile && !jarPattern.matches(path.toNIO)
        if (!isIgnored) {
          buf += path
        }
      }
    }
    Classpath(buf.toList)
  }
  override def close(): Unit = {
    try {
      server.buildShutdown().get()
      server.onBuildExit()
    } finally {
      proc.destroy()
    }
  }
}

object Bsp {
  def create(workspace: Path, files: List[Path], out: PrintStream): Bsp = {
    val bloopRoot = workspace.resolve(".bloop")
    val actualWorkspace =
      if (Files.isDirectory(workspace.resolve(".bsp"))) {
        workspace
      } else if (Files.isSymbolicLink(bloopRoot)) {
        Files.readSymbolicLink(bloopRoot).getParent()
      } else {
        workspace
      }
    val bspRoot = actualWorkspace.resolve(".bsp")
    val jsonFiles = bspRoot.toFile().listFiles()
    val json = jsonFiles.find(_.getName().endsWith(".json")).get
    val jsonText =
      new String(Files.readAllBytes(json.toPath()), StandardCharsets.UTF_8)
    val jsonElement =
      new JsonParser().parse(jsonText)
    val elems = jsonElement
      .getAsJsonObject()
      .get("argv")
      .getAsJsonArray()
      .asScala
      .map(_.getAsString())
      .toList
      .asJava
    val proc = new ProcessBuilder(elems).start()
    val tracePath = actualWorkspace.resolve("bsp.trace.json")
    val fos = Files.newOutputStream(
      tracePath,
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING
    )
    val client = new ScalafixBspClient(out)
    val launcher = new Launcher.Builder[ScalafixBspServer]()
      .traceMessages(new PrintWriter(fos))
      .setOutput(proc.getOutputStream())
      .setInput(proc.getInputStream())
      .setLocalService(client)
      .setRemoteInterface(classOf[ScalafixBspServer])
      .create()
    val listening = launcher.startListening()
    val server = launcher.getRemoteProxy()
    val bsp = new Bsp(actualWorkspace, files, proc, server)
    bsp.initialize()
    bsp
  }

}
