package scalafix.tests.rule

import coursier._
import scala.collection.JavaConverters._
import org.scalatest.FunSuite
import scalafix.testkit.DiffAssertions
import scalafix.interfaces.Scalafix
import scalafix.interfaces.ScalafixMainMode
import java.nio.file.Files
import scala.meta.internal.io.FileIO
import scala.meta.io.AbsolutePath
import java.{util => ju}
import java.nio.file.Path
import scala.meta.internal.metals.RecursivelyDelete
import scala.sys.process._

class MavenFuzzSuite extends FunSuite with DiffAssertions {
  def check(rule: String): Unit = {
    test(rule) {
      val dependencies = List(
        Dependency(
          Module(
            Organization("com.typesafe.akka"),
            ModuleName("akka-actor_2.12")
          ),
          "2.5.25"
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
      exec("git", "init")
      exec("git", "add", ".")
      exec("git", "commit", "-m", "first-commit")

      tmp.toFile().deleteOnExit()
      pprint.log(tmp)
      val paths = new ju.ArrayList[Path]()
      sources.foreach { jar =>
        FileIO.withJarFileSystem(AbsolutePath(jar.toPath()), false, true) {
          root =>
            FileIO.listAllFilesRecursively(root).files.foreach { relpath =>
              val in = root.resolve(relpath)
              val out = tmp.resolve(relpath.toString())
              Files.createDirectories(out.getParent())
              paths.add(out)
              Files.copy(in.toNIO, Files.newOutputStream(out))
            }
        }
      }
      val args = scalafix
        .newArguments()
        .withSourceroot(tmp)
        .withPaths(paths)
        .withRules(List(rule).asJava)
        .withClasspath(classfiles.map(_.toPath()).asJava)
        .withMode(ScalafixMainMode.CHECK)
      val exit = args.run()
      pprint.log(exit)
      RecursivelyDelete(AbsolutePath(tmp))
    }
  }
  check("ExplicitResultTypes")
}
