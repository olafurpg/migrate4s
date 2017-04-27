package scalafix
package rewrite

import scala.meta._
import scala.meta.semantic.v1.Signature
import scala.meta.tokens.Token._
import scala.util.matching.Regex

case class PartialToFunction(mirror: Mirror) extends SemanticRewrite(mirror) {
  val FunctionSig: Regex = "\\(Lscala/Function(\\d+);\\)I".r
  override def rewrite(ctx: RewriteCtx): Patch = {
    import ctx.tokenList._
    ctx.tree.collect {
      case Term.Apply(ref: Ref, Seq(pf @ Term.PartialFunction(Seq(kase)))) =>
        ref.symbol match {
          case Symbol.Global(_, Signature.Method(_, FunctionSig(n)))
              if n != "1" =>
            val tokens = pf.tokens
            val patches = for {
              open <- tokens.headOption.collect { case x @ LeftBrace() => x }
              close <- tokens.lastOption.collect { case x @ RightBrace() => x }
              kase <- find(open)(_.is[KwCase])
            } yield
              ctx.replaceToken(open, "(") +
                ctx.replaceToken(close, ")") +
                ctx.removeToken(kase)
            patches.asPatch
          case _ =>
            Patch.empty
        }
    }.asPatch
  }
}
