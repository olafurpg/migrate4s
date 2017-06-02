package scalafix
package rewrite

import scala.meta.{Symbol => _, _}
import scalafix.syntax._
import scalafix.util.{Whitespace => _, _}
import scala.meta.contrib._
import scala.meta.tokens.Token._
import scala.meta.semantic.Signature
import scala.collection.mutable
import org.scalameta.logger

case class NoExtendsApp(mirror: Mirror) extends SemanticRewrite(mirror) {

  override def rewrite(ctx: RewriteCtx): Patch = {
    def getTemplateCurlyTokens(template: Template): Tokens = {
      val tokens = template.tokens
      val result = for {
        close <- tokens.lastOption
        if close.is[RightBrace]
        open <- ctx.matching.open(close.asInstanceOf[RightBrace])
      } yield tokens.dropWhile(_.pos.start.offset < open.pos.start.offset)
      result.get
    }
    def indent(tokens: Tokens, n: Int): Patch =
      tokens.collect {
        case nl @ Newline() => ctx.addRight(nl, " " * n)
      }.asPatch
    ctx.tree.collect {
      case t: Defn.Object if t.templ.parents.exists {
            case c: Ctor.Ref =>
              c.symbolOpt
                .map(_.normalized.syntax)
                .contains("_root_.scala.App.")
          } && t.templ.stats.isDefined =>
        val tokens = t.templ.tokens
        val firstStatToken = t.templ.stats.get.head.tokens.head
        indent(tokens, 2) +
          ctx.addRight(tokens.last, "\n}") +
          ctx.addLeft(firstStatToken,
                      "def main(args: Seq[String]): Unit = {\n    ")
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
