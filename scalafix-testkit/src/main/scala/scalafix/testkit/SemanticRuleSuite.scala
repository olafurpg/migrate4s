package scalafix.testkit

import java.io.File
import java.nio.charset.StandardCharsets
import scala.meta.internal.io.FileIO
import org.scalatest.BeforeAndAfterAll
import org.scalatest.FunSuite
import org.scalatest.exceptions.TestFailedException
import scala.meta._
import scalafix.internal.reflect.ClasspathOps
import scalafix.internal.reflect.RuleCompiler
import scalafix.internal.testkit.AssertDiff
import scalafix.internal.testkit.CommentAssertion
import scalafix.internal.testkit.EndOfLineAssertExtractor
import scalafix.internal.testkit.MultiLineAssertExtractor

/**
  * Construct a test suite for running semantic Scalafix rules.
  *
  * @param inputClassDirectory The class directory of the input sources. This directory should contain a
  *                            META-INF/semanticd sub-directory with SemanticDB files.
  * @param inputSourceroot The source directory of the input sources. This directory should contain Scala code to
  *                        be fixed by Scalafix.
  * @param outputSourceroot The source directories of the expected output sources. These directories should contain
  *                         Scala source files with the expected output after running Scalafix. When multiple directories
  *                         are provided, the first directory that contains a source files with a matching relative path
  *                         in inputSourceroot is used.
  */
abstract class SemanticRuleSuite(
    inputClassDirectory: File,
    inputSourceroot: File,
    outputSourceroot: Seq[File]
) extends FunSuite
    with DiffAssertions
    with BeforeAndAfterAll { self =>

  private val sourceroot: AbsolutePath =
    AbsolutePath(inputSourceroot)
  private val classpath: Classpath =
    SemanticRuleSuite.defaultClasspath(AbsolutePath(inputClassDirectory))
  private val expectedOutputSourceroot: Seq[AbsolutePath] =
    outputSourceroot.map(AbsolutePath(_))

  private def scalaVersion: String = scala.util.Properties.versionNumberString
  private def scalaVersionDirectory: Option[String] =
    if (scalaVersion.startsWith("2.11")) Some("scala-2.11")
    else if (scalaVersion.startsWith("2.12")) Some("scala-2.12")
    else None

  def runOn(diffTest: RuleTest): Unit = {
    test(diffTest.filename.toString()) {
      val (rule, sdoc) = diffTest.run.apply().get
      val (fixed, messages) = rule.semanticPatch(sdoc, suppress = false)

      val tokens = fixed.tokenize.get
      val obtained = SemanticRuleSuite.stripTestkitComments(tokens)
      val candidateOutputFiles = expectedOutputSourceroot.flatMap { root =>
        val scalaSpecificFilename = scalaVersionDirectory.toList.map { path =>
          root.resolve(
            RelativePath(
              diffTest.filename.toString().replaceFirst("scala", path)))
        }
        root.resolve(diffTest.filename) :: scalaSpecificFilename
      }
      val expected = candidateOutputFiles
        .collectFirst {
          case f if f.isFile =>
            FileIO.slurp(f, StandardCharsets.UTF_8)
        }
        .getOrElse {
          if (fixed == sdoc.input.text) {
            obtained // linter
          } else {
            val tried = candidateOutputFiles.mkString("\n")
            sys.error(
              s"""Missing expected output file for test ${diffTest.filename}. Tried:
                 |$tried""".stripMargin)
          }
        }

      if (!Patch.isAllTokenPatchAtomic(patch))
        throw new TestFailedException(
          s"One or more rules used by test ${diffTest.filename} emit non-atomic " +
            "top level patches. To fix this, please use `.atomic` on individual or group " +
            "of patches that need to be atomically applied.", 0)
      val expectedLintMessages = CommentAssertion.extract(sdoc.tokens)
      val diff = AssertDiff(messages, expectedLintMessages)

      if (diff.isFailure) {
        println("###########> Lint       <###########")
        println(diff.toString)
      }

      val result = compareContents(obtained, expected)
      if (result.nonEmpty) {
        println("###########> Diff       <###########")
        println(error2message(obtained, expected))
      }

      if (result.nonEmpty || diff.isFailure) {
        throw new TestFailedException("see above", 0)
      }
    }
  }

  lazy val testsToRun = {
    val symtab = ClasspathOps
      .newSymbolTable(classpath)
      .getOrElse { sys.error("Failed to load symbol table") }
    RuleTest.fromDirectory(sourceroot, classpath, symtab)
  }
  def runAllTests(): Unit = {
    testsToRun.foreach(runOn)
  }
}

object SemanticRuleSuite {
  def defaultClasspath(classDirectory: AbsolutePath) = Classpath(
    classDirectory ::
      RuleCompiler.defaultClasspathPaths.filter(path =>
      path.toNIO.getFileName.toString.contains("scala-library"))
  )

  def stripTestkitComments(input: String): String =
    stripTestkitComments(input.tokenize.get)

  def stripTestkitComments(tokens: Tokens): String = {
    val configComment = findTestkitComment(tokens)
    tokens.filter {
      case `configComment` => false
      case EndOfLineAssertExtractor(_) => false
      case MultiLineAssertExtractor(_) => false
      case _ => true
    }.mkString
  }

  def findTestkitComment(tokens: Tokens): Token = {
    tokens
      .find { x =>
        x.is[Token.Comment] && x.syntax.startsWith("/*")
      }
      .getOrElse {
        val input = tokens.headOption.fold("the file")(_.input.toString)
        throw new IllegalArgumentException(
          s"Missing /* */ comment at the top of $input")
      }
  }

}
