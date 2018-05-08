package scalafix.internal.rule

import java.net.URI
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import org.langmeta.io.AbsolutePath
import org.langmeta.io.Classpath
import org.langmeta.io.RelativePath
import scala.collection.JavaConverters._

object DocOps {

  implicit class XtensionRelativePathScalafix(path: RelativePath) {
    // TODO: replace with RelativePath.toURI once https://github.com/scalameta/scalameta/issues/1523 is fixed
    def toRelativeURI: URI = {
      val reluri =
        path.toNIO.asScala.iterator.map(_.getFileName.toString).mkString("/")
      URI.create(URLEncoder.encode(reluri, StandardCharsets.UTF_8.name()))
    }
  }

  implicit class XtensionClasspathScalafix(cp: Classpath) {
    def resolveSemanticdb(path: RelativePath): Option[AbsolutePath] = {
      cp.shallow.find { root =>
        root.isDirectory &&
        root.resolve("META-INF").resolve("semanticdb").resolve(path).isFile
      }
    }
  }

}
