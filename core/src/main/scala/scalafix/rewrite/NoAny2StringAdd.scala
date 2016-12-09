package scalafix.rewrite

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.tokens.Token.Comment
import scala.meta.tokens.Token.LeftBrace
import scala.meta.tokens.Token.RightBrace
import scala.meta.tokens.Token.RightParen
import scala.meta.tokens.Token.Space
import scalafix.util.Patch

case object NoAny2StringAdd extends Rewrite {
  override def rewrite(ast: Tree, ctx: RewriteCtx): Seq[Patch] = {
    val api = getSemanticApi(ctx)
    def isAny2StringAdd(term: Term): Boolean =
      api.desugared(term) match {
        case Some(q"scala.Predef.any2stringadd[$_]($_)") => true
        case _ => false
      }
    val patches: Seq[Patch] = ast.collect {
      case t @ Term
            .ApplyInfix(lhs, op @ Term.Name("+"), _, Seq(Lit(str: String)))
          if isAny2StringAdd(lhs) =>
        Patch(t.tokens.head, t.tokens.last, s"""s"$${${lhs.syntax}}$str"""")
    }
    patches
  }
}
