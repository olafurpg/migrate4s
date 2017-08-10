package scalafix
package rewrite

import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta._
import scala.meta.internal.trees._

case class NoAutoApply(mirror: SemanticCtx) extends SemanticRewrite(mirror) {

  // Find the name of this Term.Apply
  @tailrec final def applyName(tree: Tree): Option[Name] = tree match {
    case Term.Apply(fun, _) => applyName(fun)
    case Term.ApplyType(fun, _) => applyName(fun)
    case Term.Select(_, n) => Some(n)
    case t @ Term.Name(_) => Some(t)
    case _ => None
  }
  override def rewrite(ctx: RewriteCtx): Patch = {
    val isApply = mutable.Set.empty[Name]
    ctx.tree
      .collect {
        case s @ Term.ApplyInfix(_, op, _, Nil) =>
          isApply += op
          None
        case s @ Term.Apply(_, Nil) =>
          applyName(s).foreach { x =>
            ctx.debug(x)
            isApply += x
          }
          None
        case name @ Term.Name(_) if !name.isBinder =>
          for {
            symbol <- mirror.symbol(name.pos)
            denot <- mirror.denotation(symbol)
            if !denot.isJavaDefined
            if denot.info.contains(s"()")
            if !isApply(name)
          } yield ctx.replaceTree(name, s"$name()")
      }
      .flatten
      .asPatch
  }
}
