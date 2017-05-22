package scalafix
package rewrite

import scala.collection.mutable
import scala.meta._

case object SignificantIndent extends Rewrite {

  def getMatchingBraces(tokens: Tokens) = {
    val matchingParens = mutable.Map.empty[Token.LeftBrace, Token.RightBrace]
    var stack = List.empty[Token.LeftBrace]
    tokens.foreach {
      case open @ Token.LeftBrace() =>
        stack = open :: stack
      case close @ Token.RightBrace() =>
        val open :: nextStack = stack
        matchingParens(open) = close
        stack = nextStack
      case _ =>
    }
    matchingParens
  }

  override def rewrite(ctx: RewriteCtx): Patch = {
    val tokens = ctx.tree.tokens
    val matchingBraces = getMatchingBraces(tokens)
    tokens.collect {
      case open @ Token.LeftBrace() =>
        val close = matchingBraces(open)
        if (close.pos.start.line == open.pos.start.line) Patch.empty
        else {
          ctx.removeToken(open, trim = true) +
            ctx.removeToken(close, trim = true)
        }
    }.asPatch
  }
}
