package scalafix
package rewrite
import scala.meta._
import scala.util.control.NonFatal
import scalafix.config.MetaconfigPendingUpstream
import scalafix.dotc.DottyCompiler
import scalafix.dotc.DottyMirror
import scalafix.syntax._
import metaconfig.ConfError
import metaconfig.Configured
import metaconfig.Configured.Ok
import org.scalameta.logger

case class ExplicitImplicitArgs(mirror: ScalafixDatabase)
    extends SemanticRewrite(mirror) {
  override def init: Configured[Rewrite] = {
    try {
      val dottyMirror =
        DottyMirror.fromScalacMirror(mirror, DottyCompiler().get)
      Ok(ExplicitImplicitArgs2(mirror, dottyMirror))
    } catch {
      case NonFatal(e) =>
        MetaconfigPendingUpstream.fromException(e)
    }
  }

  override def rewrite(ctx: RewriteCtx): Patch = Patch.empty
}

case class ExplicitImplicitArgs2(scalacMirror: ScalafixDatabase,
                                 dotcMirror: ScalafixDatabase)
    extends SemanticRewrite(scalacMirror) {
  val sugars = scalacMirror.database.sugars.map { case (a, b) => a.start -> b }
  override def rewrite(ctx: RewriteCtx): Patch = {
    logger.elem(dotcMirror.entries.head._1)
    val tokensByPosition = ctx.tokens.map(x => x.pos.start -> x).toMap
    sugars.collect {
      case (p, s)
          if p.input == ctx.input &&
            tokensByPosition.contains(p) &&
            s.startsWith("(") =>
        ctx.addLeft(tokensByPosition(p), s)
    }.asPatch
  }
}
