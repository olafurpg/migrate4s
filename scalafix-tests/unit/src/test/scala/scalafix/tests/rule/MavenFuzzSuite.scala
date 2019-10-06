package scalafix.tests.rule

import coursier._
import scala.collection.JavaConverters._
import org.scalatest.FunSuite
import scalafix.testkit.DiffAssertions
import scalafix.interfaces.Scalafix
import java.nio.file.Files
import scala.meta.internal.io.FileIO
import scala.meta.io.AbsolutePath
import java.{util => ju}
import java.nio.file.Path
import scala.sys.process._
import scala.meta.internal.pc.ScalaPresentationCompiler
import scala.collection.mutable
import scala.tools.nsc.reporters.StoreReporter
import scala.meta.internal.pc.MetalsGlobal
import scala.reflect.io.VirtualFile
import java.nio.charset.StandardCharsets
import com.google.common.cache.AbstractCache.StatsCounter

class MavenFuzzSuite extends FunSuite with DiffAssertions {
  private def getCompilingSources(
      g: MetalsGlobal,
      classfiles: Seq[Path],
      sourceJars: Seq[Path],
      tmp: Path
  ): Seq[Path] = {
    val result = mutable.ArrayBuffer.empty[Path]
    sourceJars.foreach { jar =>
      FileIO.withJarFileSystem(AbsolutePath(jar), false, true) { root =>
        FileIO.listAllFilesRecursively(root).files.foreach { relpath =>
          val in = root.resolve(relpath)
          val text = FileIO.slurp(in, StandardCharsets.UTF_8)
          val errors = compileErrors(g, text, relpath.toString())
          if (errors.isEmpty) {
            val out = tmp.resolve(relpath.toString())
            Files.createDirectories(out.getParent())
            val stream = Files.newOutputStream(out)
            try Files.copy(in.toNIO, stream)
            finally stream.close()
            result += out
          }
        }
      }
    }
    result
  }

  def compileErrors(
      g: MetalsGlobal,
      code: String,
      filename: String
  ): List[String] = {
    val reporter = new StoreReporter()
    val run = new g.Run()
    val source = g.newSourceFile(code, filename)
    run.compileSources(List(source))
    val errors = reporter.infos.filter(_.severity.id == 2)
    errors.toList.map(_.toString())
  }

  def check(rule: String): Unit = {
    test(rule) {
      val dependencies = List(
        Dependency(
          Module(
            Organization("org.scalameta"),
            ModuleName("metals_2.12")
          ),
          "0.7.6"
        )
      )
      val fetch = Fetch()
      val classfiles = fetch
        .withDependencies(dependencies)
        .run()
        .map(_.toPath())
      val sources = fetch
        .withClassifiers(Set(Classifier("sources")))
        .withDependencies(dependencies.map(_.withTransitive(false)))
        .run()
        .map(_.toPath())
      val scalafix =
        Scalafix.classloadInstance(this.getClass().getClassLoader())
      val tmp = Files.createTempDirectory("scalafix")
      def exec(cmd: String*): Unit = {
        val gitinit = Process(cmd.toList, Some(tmp.toFile())).!
        require(gitinit == 0, gitinit)
      }

      tmp.toFile().deleteOnExit()
      val g = ScalaPresentationCompiler(classpath = classfiles)
        .newCompiler()
      val paths = getCompilingSources(g, classfiles, sources, tmp)
      // exec("git", "init")
      // exec("git", "add", ".")
      // exec("git", "commit", "-m", "first-commit")
      val args = scalafix
        .newArguments()
        .withSourceroot(tmp)
        .withPaths(paths.asJava)
        .withRules(List(rule).asJava)
        .withClasspath(classfiles.asJava)
      // .withMode(ScalafixMainMode.CHECK)
      val exit = args.run()
      pprint.log(exit)
      // exec("git", "diff")
      FileIO.listAllFilesRecursively(AbsolutePath(tmp)).foreach { path =>
        val text = FileIO.slurp(path, StandardCharsets.UTF_8)
        val errors = compileErrors(g, text, path.toString())
        if (errors.nonEmpty) {
          pprint.log(path)
          pprint.log(errors)
        }
      }
      pprint.log(tmp)
    }
  }
  check("ExplicitResultTypes")
}
