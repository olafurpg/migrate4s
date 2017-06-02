package scalafix
package rewrite

import scala.meta.{ Symbol => _, _ }
import scalafix.syntax._
import scalafix.util.{ Whitespace => _, _ }
import scala.meta.contrib._
import scala.meta.tokens.Token._
import scala.meta.semantic.Signature
import scala.collection.mutable

case class NoExtendsApp(mirror: Mirror) extends SemanticRewrite(mirror) {

  override def rewrite(ctx: RewriteCtx): Patch = {
    ctx.tree.collect {
      case t: Defn.Object
        if t.templ.parents.exists { case c: Ctor.Ref => c.symbolOpt.map(_.normalized.syntax) == Some("_root_.scala.App.") } && t.templ.stats.isDefined =>
          println(t.templ.tokens.structure)
          val (head, stats) = t.templ.tokens.span(_.is[LeftBrace])
          println(head)
          println(stats)
          val indentation = t.templ.stats.get.head.tokens.takeWhile(_.is[Whitespace])
          println(s">>>>>${indentation}<<<<")
          val open = ctx.addLeft(t.templ.stats.get.head.tokens.head, "def main(args: Array[String]) = {\n  ")
          val close = ctx.addRight(t.templ.stats.get.last.tokens.last, "\n}")
          val indent = t.templ.stats.get.foldLeft(Patch.empty)((patch, s) => patch + s.tokens.collect { case tok @ Newline() => ctx.addRight(tok, "  ") }.asPatch)
          open + close + indent
    }.asPatch
  }
}


// object Foo {
//   println()
// }

// object Foo {
//   def foo() = {
//     println()
//   }
// }

