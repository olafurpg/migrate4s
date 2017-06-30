package scalafix.reflect

import scala.meta._
import scalafix.Rewrite
import scalafix.config._
import scalafix.util.FileOps
import java.io.File
import java.net.URL
import scalafix.rewrite.ScalafixDatabase
import metaconfig.Conf
import metaconfig.ConfDecoder

object ScalafixCompilerDecoder {
  def syntactic: ConfDecoder[Rewrite] = fromMirrorOption(None)
  def semantic(mirror: ScalafixDatabase): ConfDecoder[Rewrite] =
    fromMirrorOption(Some(mirror))
  def fromMirrorOption(
      mirror: Option[ScalafixDatabase]): ConfDecoder[Rewrite] =
    rewriteConfDecoder(
      MetaconfigPendingUpstream.orElse(baseCompilerDecoder(mirror),
                                       baseRewriteDecoders(mirror)),
      mirror)
  def baseCompilerDecoder(
      mirror: Option[ScalafixDatabase]): ConfDecoder[Rewrite] =
    ConfDecoder.instance[Rewrite] {
      case FromSourceRewrite(code) =>
        ScalafixToolbox.getRewrite(code, mirror)
    }

  object UrlRewrite {
    def unapply(arg: Conf.Str): Option[URL] = arg match {
      case UriRewrite("http" | "https", uri) if uri.isAbsolute =>
        Option(uri.toURL)
      case GitHubUrlRewrite(url) => Option(url)
      case _ => None
    }
  }

  object GitHubUrlRewrite {
    private[this] val GitHubShorthand =
      """github:([^\/]+)\/([^\/]+)\/([^\/]+)""".r
    private[this] val GitHubShorthandWithSha =
      """github:([^\/]+)\/([^\/]+)\/([^\/]+)\?sha=(.+)""".r

    private[this] val alphanumerical = "[^a-zA-Z0-9]"

    // approximates the "format=Camel" formatter in giter8.
    // http://www.foundweekends.org/giter8/Combined+Pages.html#Formatting+template+fields
    private[this] def CamelCase(string: String): String =
      string.split(alphanumerical).map(_.capitalize).mkString

    // approximates the "format=Snake" formatter in giter8.
    private[this] def SnakeCase(string: String): String =
      string.split(alphanumerical).map(_.toLowerCase).mkString("_")

    private[this] def expandGitHubURL(org: String,
                                      repo: String,
                                      version: String,
                                      sha: String): URL = {
      val fileName = s"${CamelCase(repo)}_${SnakeCase(version)}.scala"
      new URL(
        s"https://github.com/$org/$repo/blob/$sha/scalafix/rewrites/src/main/scala/fix/$fileName")
    }

    def unapply(arg: Conf.Str): Option[URL] = arg.value match {
      case GitHubShorthandWithSha(org, repo, version, sha) =>
        Option(expandGitHubURL(org, repo, version, sha))
      case GitHubShorthand(org, repo, version) =>
        Option(expandGitHubURL(org, repo, version, "master"))
      case _ => None
    }
  }

  object FileRewrite {
    def unapply(arg: Conf.Str): Option[File] = arg match {
      case UriRewrite("file", uri) =>
        Option(new File(uri.getSchemeSpecificPart).getAbsoluteFile)
      case _ => None
    }
  }

  object FromSourceRewrite {
    def unapply(arg: Conf.Str): Option[Input] = arg match {
      case FileRewrite(file) => Option(Input.File(file))
      case UrlRewrite(url) =>
        val code = FileOps.readURL(url)
        val file = File.createTempFile(url.toString, ".scala")
        FileOps.writeFile(file, code)
        Option(Input.File(file))
      case _ => None
    }
  }

}
