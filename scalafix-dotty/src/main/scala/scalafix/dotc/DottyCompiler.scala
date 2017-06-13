package scalafix
package dotc

import java.io.File
import java.lang.reflect.InvocationHandler
import java.lang.reflect.Method
import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Optional
import scala.meta._
import scala.util.Try
import scalafix.syntax._
import dotty.tools.dotc.interfaces._
import org.scalameta.logger

case class Error(message: String, position: Option[SourcePosition])

class DottyCompiler(classloader: URLClassLoader) {
  val sourceroot: Path = Files.createTempDirectory("scalafix.DottyCompiler")
  private val cp = classloader.getURLs
    .collect { case url if url.getPath.endsWith(".jar") => url.getPath }
    .mkString(File.pathSeparator)
  private val mainClass =
    Class.forName("dotty.tools.dotc.Main", false, classloader)
  //  NOTE: can't use classOf[SimpleReporter] since those classes origin
  // from a different classloader.
  private val reporterClass =
    classloader.loadClass(classOf[SimpleReporter].getName)
  private val callbackClass =
    classloader.loadClass(classOf[CompilerCallback].getName)
  private val driver =
    classloader.loadClass("dotty.tools.dotc.Main$").newInstance()
  private val process = mainClass.getDeclaredMethod("process",
                                                    classOf[Array[String]],
                                                    reporterClass,
                                                    callbackClass)
  // wraps an instance of interfaced.Diagnostic that was classloaded from
  // the dotty classloader into an instance of interfaces.Diagnostic from
  // the current classloader.
  def reflectiveDiagnostic(any: AnyRef): Diagnostic = {
    import scala.language.reflectiveCalls
    type DynSourcePosition = {
      def lineContent: String
      def point: Int
      def line: Int
      def column: Int
      def start: Int
      def startLine: Int
      def startColumn: Int
      def end: Int
      def endLine: Int
      def endColumn: Int
      def source: DynSourceFile
    }
    type DynSourceFile = {
      def name: String
      def path: String
      def jfile: Optional[File]
      def content: Array[Char]
    }
    type DynDiagnostic = {
      def message: String
      def level: Int
      def position: Optional[DynSourcePosition]
    }
    val diagnostic = any.asInstanceOf[DynDiagnostic]
    val result = new Diagnostic {
      override def level() = diagnostic.level
      override def position() = {
        val posOpt = diagnostic.position
        if (!posOpt.isPresent) Optional.empty[SourcePosition]
        else {
          val pos = posOpt.get()
          val sourcePos = new SourcePosition {
            override def endColumn() = pos.endColumn
            override def column() = pos.column
            override def endLine() = pos.endLine
            override def source() = new SourceFile {
              private val s = pos.source
              override def content(): Array[Char] = s.content
              override def jfile(): Optional[File] = s.jfile
              override def name(): String = s.name
              override def path(): String = s.path
            }
            override def startLine() = pos.startLine
            override def line() = pos.line
            override def point() = pos.point
            override def startColumn() = pos.startColumn
            override def end() = pos.end
            override def start() = pos.start
            override def lineContent() = pos.lineContent
          }
          Optional.of(sourcePos)
        }
      }
      override def message() = diagnostic.message
    }
    result
  }

  def compile(code: Input, classpath: Seq[String] = Nil): List[Diagnostic] =
    compile(List(code), classpath)

  def compile(inputs: Seq[Input], classpath: Seq[String]): List[Diagnostic] = {
    val out = Files.createTempDirectory("scalafix").toString
    val tmpFiles = inputs.map { input =>
      val relpath: Path = input match {
        case Input.File(path, _) => path.toNIO
        case Input.LabeledString(path, _) => Paths.get(path)
        case _ => Files.createTempFile(sourceroot, "scalafix", "input.scala")
      }
      val tmpFile = sourceroot.resolve(relpath)
      tmpFile.getParent.toFile.mkdirs()
      tmpFile.toFile.createNewFile()
      Files.write(tmpFile, new String(input.chars).getBytes)
      tmpFile.toAbsolutePath.toString
    }
    val buffer = List.newBuilder[Diagnostic]
    val args = Array(
      "-classpath",
      (cp +: classpath).mkString(File.pathSeparator),
      "-d",
      out
    ) ++ tmpFiles
    val simpleReporterHandler = new InvocationHandler {
      override def invoke(proxy: scala.Any,
                          method: Method,
                          args: Array[AnyRef]) = {
        buffer += reflectiveDiagnostic(args.head)
      }
    }
    val reporterProxy =
      java.lang.reflect.Proxy.newProxyInstance(classloader,
                                               Array(reporterClass),
                                               simpleReporterHandler)
    process.invoke(driver, args, reporterProxy, null)
    buffer.result()
  }
}

object DottyCompiler {
  def defaultVersion: String =
    sys.env.getOrElse("DOTTY_VERSION", Versions.dottyVersion)
  def apply(version: String = defaultVersion): Try[DottyCompiler] =
    for {
      classloader <- DottyResolver.getDottyClassloader(version)
      compiler <- Try(new DottyCompiler(classloader))
    } yield compiler
}

object DottyResolver {
  import coursier._
  def getDottyClassloader(version: String): Try[URLClassLoader] = Try {
    val binaryVersion = version.split("\\.").take(2).mkString(".")
    val start = Resolution(
      Set(
        Dependency(Module("ch.epfl.lamp", s"dotty-compiler_$binaryVersion"),
                   version)
      )
    )
    val repositories = Seq(
      Cache.ivy2Local,
      MavenRepository("https://repo1.maven.org/maven2")
    )
    val fetch = Fetch.from(repositories, Cache.fetch())
    val resolution = start.process.run(fetch).unsafePerformSync
    val errors: Seq[((Module, String), Seq[String])] =
      resolution.metadataErrors
    if (errors.nonEmpty) sys.error(errors.toString)

    import java.io.File
    import scalaz.concurrent.Task

    val localArtifacts: Seq[Either[FileError, File]] = Task
      .gatherUnordered(
        resolution.artifacts.map(Cache.file(_).run)
      )
      .unsafePerformSync
      .map(_.toEither)
    val errs =
      localArtifacts.collect { case Left(file) => file }
    if (errs.nonEmpty) sys.error(errs.toString())
    else {
      val okFiles =
        localArtifacts.collect { case Right(file) => file }
      new URLClassLoader(okFiles.map(_.toURI.toURL).toArray, null)
    }
  }
}
