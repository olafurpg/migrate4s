package scalafix
package rewrite

import scala.collection.mutable
import scala.meta._
import scala.meta.internal.trees._
import scalafix.util.TreeExtractors

case class NoAutoApply(mirror: SemanticCtx) extends SemanticRewrite(mirror) {

  override def rewrite(ctx: RewriteCtx): Patch = {
    var patch = Patch.empty
    val fixed = mutable.Set.empty[Name]
    def fix(
        fun: Term,
        parent: Tree,
        targs: List[Type],
        args: List[List[Term]]): Unit = {
      for {
        name <- TreeExtractors.RelevantName.unapply(fun)
        _ = if (args.nonEmpty) fixed += name
        if name.isReference
        if !name.parent.exists(_.is[Term.ApplyInfix])
        if !fixed(name)
        symbol <- mirror.symbol(name)
        denot <- mirror.denotation(symbol)
        if !denot.isJavaDefined
        // TODO(olafur) replace this denot.info.ends/startsWith hack with
        // Type.Method once it's available.
        if (denot.info.startsWith(s"()") &&
          !denot.info.startsWith(s"() =>")) ||
          denot.info.contains("]()")
      } {
        val toAdd = if (targs.isEmpty) name else parent
        patch += ctx.addRight(toAdd, s"()")
        fixed += name
      }
    }
    new Traverser {
      override def apply(tree: Tree): Unit = {
        tree match {
          case t @ q"$fun[..$targs](...${args})" =>
            fix(fun, t, targs, args)
          case t @ q"$fun(...${args})" =>
            fix(fun, t, Nil, args)
          case _ =>
        }
        super.apply(tree)
      }
    }.apply(ctx.tree)
    patch
  }
}
