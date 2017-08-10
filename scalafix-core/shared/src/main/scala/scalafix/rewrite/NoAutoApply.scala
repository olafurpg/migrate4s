package scalafix
package rewrite

import scala.meta._
import scala.meta.internal.trees._

case class NoAutoApply(mirror: SemanticCtx) extends SemanticRewrite(mirror) {
  override def rewrite(ctx: RewriteCtx): Patch = {
    ctx.tree
      .collect {
        case name @ Term.Name(_) if !name.isBinder =>
          for {
            symbol <- mirror.symbol(name.pos)
            denot <- mirror.denotation(symbol)
            if !denot.isJavaDefined
            if denot.info.startsWith(s"()")
            if !name.parent.exists(_.is[Term.Apply])
          } yield ctx.replaceTree(name, s"$name()")
      }
      .flatten
      .asPatch
  }
}
