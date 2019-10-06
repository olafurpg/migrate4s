package scalafix.tests.rule

import coursier._
import scala.collection.JavaConverters._
import org.scalatest.FunSuite
import scalafix.testkit.DiffAssertions
import scalafix.interfaces.Scalafix
import scalafix.interfaces.ScalafixMainMode

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
      val fetch = Fetch().withDependencies(dependencies)
      val classfiles = fetch.run()
      val sources = fetch
        .withClassifiers(Set(Classifier("sources")))
        .run()
      pprint.log(classfiles)
      pprint.log(sources)
      val scalafix =
        Scalafix.classloadInstance(this.getClass().getClassLoader())
      val args = scalafix
        .newArguments()
        .withRules(List(rule).asJava)
        .withPaths(sources.map(_.toPath()).asJava)
        .withClasspath(classfiles.map(_.toPath()).asJava)
        .withMode(ScalafixMainMode.CHECK)
      val exit = args.run()
      pprint.log(exit)
    }
  }
  check("ExplicitResultTypes")
}
