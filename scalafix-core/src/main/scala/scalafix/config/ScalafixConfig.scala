package scalafix
package config

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.parsers.Parse
import scalafix.rewrite.ScalafixMirror

import java.io.File

import metaconfig._
import metaconfig.typesafeconfig.TypesafeConfig2Class

@DeriveConfDecoder
case class ScalafixConfig(
    rewrite: Rewrite = Rewrite.empty,
    parser: Parse[_ <: Tree] = Parse.parseSource,
    @Recurse imports: ImportsConfig = ImportsConfig(),
    @Recurse patches: PatchConfig = PatchConfig(),
    @Recurse debug: DebugConfig = DebugConfig(),
    fatalWarnings: Boolean = true,
    reporter: ScalafixReporter = ScalafixReporter.default,
    dialect: Dialect = Scala211
) {
  implicit val RewriteConfDecoder: ConfDecoder[Rewrite] =
    Rewrite.syntaxRewriteConfDecoder
  def withRewrite(newRewrite: Rewrite): ScalafixConfig =
    copy(rewrite = rewrite andThen rewrite)
}

object ScalafixConfig {

  val default = ScalafixConfig()
  val syntaxConfDecoder: ConfDecoder[ScalafixConfig] = default.reader

  /** Returns config from current working directory, if .scalafix.conf exists. */
  def auto(workingDir: File,
           mirror: Option[ScalafixMirror]): Option[ScalafixConfig] = {
    val file = new File(workingDir, ".scalafix.conf")
    if (file.isFile && file.exists())
      Some(ScalafixConfig.fromFile(file, mirror).get)
    else None
  }

  private def gimmeClass[T](conf: Configured[Conf])(
      implicit reader: metaconfig.ConfDecoder[T]): metaconfig.Configured[T] =
    for {
      config <- conf
      cls <- reader.read(config)
    } yield cls

  def fromFile(file: File,
               mirror: Option[ScalafixMirror]): Configured[ScalafixConfig] =
    gimmeClass(TypesafeConfig2Class.gimmeConfFromFile(file))(
      scalafixConfigConfDecoder(mirror))

  def fromString(str: String,
                 mirror: Option[ScalafixMirror]): Configured[ScalafixConfig] =
    gimmeClass(TypesafeConfig2Class.gimmeConfFromString(str))(
      scalafixConfigConfDecoder(mirror))
}
