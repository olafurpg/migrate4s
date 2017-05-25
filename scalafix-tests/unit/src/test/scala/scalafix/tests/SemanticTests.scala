package scalafix.tests

import scala.meta._
import scalafix.testkit._
import org.scalameta.logger

class SemanticTests
    extends SemanticRewriteSuite(
      Database.load(Classpath(AbsolutePath(BuildInfo.mirrorClasspath))),
      AbsolutePath(BuildInfo.inputSourceroot),
      AbsolutePath(BuildInfo.outputSourceroot)
    ) {
  // directory containing .source files
//  DiffTest.fromFile(BuildInfo.testsInputResources).foreach(runDiffTest)
  logger.elem(mirror.database)
}
