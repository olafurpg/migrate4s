package scalafix
package rewrite

import scala.meta._
import scala.meta.internal.trees._

case class NoAutoApply(mirror: Mirror) extends SemanticRewrite(mirror) {
  override def rewrite(ctx: RewriteCtx): Patch = {
    ctx.tree
      .collect {
        case name @ Term.Name(_) if !name.isBinder =>
          for {
            symbol <- mirror.database.names.get(name.pos)
            denot <- mirror.database.denotations.get(symbol)
            if denot.info.startsWith(s"()")
            _ = ctx.debug(denot, denot)
            if !name.parent.exists(_.is[Term.Apply])
          } yield ctx.replaceTree(name, s"$name()")
      }
      .flatten
      .asPatch
  }
}
