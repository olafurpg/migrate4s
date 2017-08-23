package scalafix
package rewrite

import scalafix.syntax._
import scala.collection.mutable
import scala.collection.mutable
import scala.meta._
import scala.meta.internal.trees._
import scalafix.util.TreeExtractors

case class NoAutoApply(mirror: SemanticCtx) extends SemanticRewrite(mirror) {
  val isMagicSymbol: Set[Symbol] = Set(
    Symbol("_root_.scala.Any#"),
    Symbol("_root_.java.lang.Object#")
  )

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
        if !isMagicSymbol(symbol.owner)
        denot <- mirror.denotation(symbol)
        if !denot.isJavaDefined
        // TODO(olafur) replace denot.info.ends/startsWith hack once
        // Type.Method once it's available
        if (denot.info.startsWith(s"()") &&
          !denot.info.startsWith(s"() =>")) ||
          denot.info.contains("]()")
        _ = if (denot.name == "toString") ctx.debug(denot.info, symbol)
        _ = if (denot.name == "hashCode") ctx.debug(denot.info, symbol)
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
