package scalafix
package cli

import scalafix.config.ScalafixConfig
import scalafix.config.ScalafixMetaconfigReaders

import java.io.InputStream
import java.io.PrintStream

import caseapp._
import caseapp.core.{Parser, ArgParser, Messages}
import metaconfig.Conf

object ArgParserImplicits {
  implicit val inputStreamRead: ArgParser[InputStream] =
    ArgParser.instance[InputStream]("stdin")(_ => Right(System.in))
  implicit val printStreamRead: ArgParser[PrintStream] =
    ArgParser.instance[PrintStream]("stdout")(_ => Right(System.out))
  lazy val OptionsParser: Parser[CliCommand] = ??? // Parser.apply[CliCommand]
  lazy val OptionsMessages: Messages[CliCommand] = ??? // Messages[CliCommand]
}


sealed trait Bar
case class A(
    bar: String
)
case class B(
    kas: String
)

object BarApp extends CaseApp[Bar] {
  override def run(options: A, remainingArgs: RemainingArgs): Unit = ???
}
