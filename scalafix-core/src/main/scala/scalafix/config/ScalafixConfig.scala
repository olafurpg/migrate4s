package scalafix
package config

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.parsers.Parse
import scalafix.rewrite.ScalafixRewrite

import java.io.File

import metaconfig._
import metaconfig.typesafeconfig.TypesafeConfig2Class

@DeriveConfDecoder
case class ScalafixConfig(
    parser: Parse[_ <: Tree] = Parse.parseSource,
    @Recurse imports: ImportsConfig = ImportsConfig(),
    @Recurse patches: PatchConfig = PatchConfig(),
    @Recurse debug: DebugConfig = DebugConfig(),
    fatalWarnings: Boolean = true,
    reporter: ScalafixReporter = ScalafixReporter.default,
    dialect: Dialect = Scala211
)

object ScalafixConfig {

  val default = ScalafixConfig()
  implicit val ScalafixConfigDecoder: ConfDecoder[ScalafixConfig] =
    default.reader

  /** Returns config from current working directory, if .scalafix.conf exists. */
  def auto(workingDir: File): Option[ConfigAndRewrite] = {
    val file = new File(workingDir, ".scalafix.conf")
    if (file.isFile && file.exists()) {
      Some(ScalafixConfig.fromFile(file).get)
    } else None
  }

  private def gimmeClass[A, B](conf: Configured[Conf])(
      implicit reader: metaconfig.ConfDecoder[A],
      readerB: metaconfig.ConfDecoder[B]
  ): metaconfig.Configured[(A, B)] =
    for {
      config <- conf
      cls <- reader.read(config)
      clsB <- readerB.read(config)
    } yield cls -> clsB

  def fromFile(file: File): Configured[ConfigAndRewrite] =
    gimmeClass(TypesafeConfig2Class.gimmeConfFromFile(file))

  def fromString(str: String): Configured[ConfigAndRewrite] =
    gimmeClass(TypesafeConfig2Class.gimmeConfFromString(str))
}
