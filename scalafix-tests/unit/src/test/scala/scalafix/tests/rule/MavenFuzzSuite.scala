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

class MavenFuzzSuite extends FunSuite with DiffAssertions {
  private def getCompilingSources(
      classfiles: Seq[Path],
      sourceJars: Seq[Path],
      tmp: Path
  ): Seq[Path] = {
    val result = mutable.ArrayBuffer.empty[Path]
    val g = ScalaPresentationCompiler(classpath = classfiles).newCompiler()
    sourceJars.foreach { jar =>
      FileIO.withJarFileSystem(AbsolutePath(jar), false, true) { root =>
        FileIO.listAllFilesRecursively(root).files.foreach { relpath =>
          val in = root.resolve(relpath)
          val out = tmp.resolve(relpath.toString())
          Files.createDirectories(out.getParent())
          val stream = Files.newOutputStream(out)
          try Files.copy(in.toNIO, stream)
          finally stream.close()
          val reporter = new StoreReporter()
          val run = new g.Run()
          run.compile(List(out.toAbsolutePath().toString()))
          val errors = reporter.infos.filter(_.severity.id == 2)
          if (errors.isEmpty) {
            result += out
            pprint.log(out)
          } else {
            pprint.log(reporter.infos)
          }
        }
      }
    }
    result
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
      val sources = fetch
        .withClassifiers(Set(Classifier("sources")))
        .withDependencies(dependencies.map(_.withTransitive(false)))
        .run()
      val scalafix =
        Scalafix.classloadInstance(this.getClass().getClassLoader())
      val tmp = Files.createTempDirectory("scalafix")
      def exec(cmd: String*): Unit = {
        val gitinit = Process(cmd.toList, Some(tmp.toFile())).!
        require(gitinit == 0, gitinit)
      }

      tmp.toFile().deleteOnExit()
      val paths = getCompilingSources(
        classfiles.map(_.toPath()),
        sources.map(_.toPath()),
        tmp
      )
      // exec("git", "init")
      // exec("git", "add", ".")
      // exec("git", "commit", "-m", "first-commit")
      val args = scalafix
        .newArguments()
        .withSourceroot(tmp)
        .withPaths(paths.asJava)
        .withRules(List(rule).asJava)
        .withClasspath(classfiles.map(_.toPath()).asJava)
      // .withMode(ScalafixMainMode.CHECK)
      val exit = args.run()
      pprint.log(exit)
      // exec("git", "diff")
      pprint.log(classfiles.filter(_.toString().contains("metals")))
      pprint.log(tmp)
      // RecursivelyDelete(AbsolutePath(tmp))
    }
  }
  check("ExplicitResultTypes")
}
