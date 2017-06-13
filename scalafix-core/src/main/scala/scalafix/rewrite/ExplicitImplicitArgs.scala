package scalafix
package rewrite

import scala.meta._
import scalafix.syntax._
import org.scalameta.logger

case class ExplicitImplicitArgs(mirror: Mirror)
    extends SemanticRewrite(mirror) {
  val sugars = mirror.database.sugars.map { case (a, b) => a.start -> b }

  override def rewrite(ctx: RewriteCtx): Patch = {
    val d = Database(mirror.database.entries.filter(_._1 == ctx.input))
    logger.elem(d)
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
