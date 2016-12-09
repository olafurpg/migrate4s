package scalafix.rewrite

import scala.meta._
import scala.meta.tokens.Token.Comment
import scala.meta.tokens.Token.LeftBrace
import scala.meta.tokens.Token.RightBrace
import scala.meta.tokens.Token.RightParen
import scala.meta.tokens.Token.Space
import scalafix.util.Patch
import scalafix.util.Whitespace

case object NoPostfix extends Rewrite {
  override def rewrite(ast: Tree, ctx: RewriteCtx): Seq[Patch] = {
    import ctx.tokenList._
    val patches: Seq[Patch] = ast.collect {
      case Term.Select(_, name)
          if revFind(name.tokens.head)(!_.is[Whitespace])
            .exists(!_.is[Token.Dot]) =>
        val end = name.tokens.head
        val start =
          if (prev(end).is[Space]) prev(end)
          else end
        Patch(start, end, s".$end")
    }
    patches
  }
}
