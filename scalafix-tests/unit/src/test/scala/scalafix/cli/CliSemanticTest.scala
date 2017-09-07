package scalafix.cli

import scala.collection.immutable.Seq
import scalafix.internal.cli.CommonOptions
import scalafix.tests.BuildInfo

class CliSemanticTest extends BaseCliTest {

  checkSemantic(
    name = "--classpath inference error",
    args = Nil,
    expectedExit = ExitStatus.InvalidCommandLineOption,
    outputAssert = output => {
      assert(output.contains("Unable to infer --classpath"))
    }
  )

  checkSemantic(
    name = "--classpath explicit is OK",
    args = Seq(
      "--classpath",
      BuildInfo.semanticClasspath.getAbsolutePath
    ),
    expectedExit = ExitStatus.Ok
  )

  checkSemantic(
    name = "--sourceroot is not a file",
    args = Seq(
      "--sourceroot",
      "bogus",
      "--classpath",
      BuildInfo.semanticClasspath.getAbsolutePath
    ),
    expectedExit = ExitStatus.InvalidCommandLineOption,
    outputAssert = msg => assert(msg.contains("Invalid --sourceroot"))
  )

  checkSemantic(
    name = "--sourceroot points to non-existing file",
    args = Seq(
      "--sourceroot",
      CommonOptions.default.workingDirectory,
      "--classpath",
      BuildInfo.semanticClasspath.getAbsolutePath
    ),
    expectedExit = ExitStatus.InvalidCommandLineOption,
    outputAssert = msg => {
      assert(msg.contains("is not a file"))
      assert(msg.contains("Is --sourceroot correct?"))
    }
  )

  checkSemantic(
    name = "--sourceroot does not resolve all --files",
    args = Seq(
      "--sourceroot",
      CommonOptions.default.workingPath
        .resolve("scalafix-tests")
        .resolve("input")
        .resolve("src")
        .resolve("main")
        .toString(),
      "--classpath",
      BuildInfo.semanticClasspath.getAbsolutePath
    ),
    expectedExit = ExitStatus.InvalidCommandLineOption,
    outputAssert = msg => {
      assert(msg.contains("No semanticdb associated with"))
      assert(msg.contains(removeImportsPath.toString()))
      assert(msg.contains("Is --sourceroot correct?"))
    }
  )

  checkSemantic(
    name = "ignore stale semanticdb if contents would be unchanged",
    args = Seq(),
    expectedExit = ExitStatus.InvalidCommandLineOption,
    run = runMain => {
      runMain.apply()
      runMain.apply() // run it twice
    },
    outputAssert = msg => {
      assert(msg.isEmpty)
    }
  )
}
