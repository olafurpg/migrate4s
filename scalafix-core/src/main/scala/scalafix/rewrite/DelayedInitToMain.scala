package scalafix
package rewrite

import scala.meta._
import org.scalameta.logger
import scalafix.syntax._

case class DelayedInitToMain(mirror: Mirror) extends SemanticRewrite(mirror) {
  override def rewrite(ctx: RewriteCtx): Patch = {
    logger.elem(ctx.tree.structure,
                mirror.database.entries.filter(_._1 == ctx.tree.input))
    ctx.tree.collect {
      case Defn.Object(_, _, Template(_, parents, _, _)) =>
    }
    Patch.empty
  }
}
