package scalafix
package rewrite

import scala.meta._
import scalafix.util.Newline

case class ExpandSugars(mirror: Mirror) extends SemanticRewrite(mirror) {
  override def rewrite(ctx: RewriteCtx): Patch = {
    val tokenByEndPos = ctx.tokens.toIterator.map(t => t.end -> t).toMap
    val tokenByStartPos = ctx.tokens.toIterator.map(t => t.start -> t).toMap
    ctx.mirror.database.sugars
      .collect {
        case (pos, sugar) if pos.start == pos.end =>
          for {
            token <- tokenByEndPos.get(pos.start)
          } yield {
            val toEdit =
              if (token.is[Newline])
                token
              else token
            if (sugar.input.value.startsWith("*")) {
              ctx.addRight(toEdit, sugar.input.value.stripPrefix("*"))
            } else
              Patch.empty
          }
        case (pos, sugar) =>
          val sugarTokens = sugar.input.tokenize.get
          for {
            start <- tokenByStartPos.get(pos.start)
            end <- tokenByEndPos.get(pos.end)
            star <- sugarTokens.find {
              case token @ Token.Ident("*") =>
                mirror.database.names.contains(token.pos)
              case _ => false
            }
          } yield {
            val (before, after) = {
              sugar.input.value.substring(0, star.pos.start) ->
                sugar.input.value.substring(star.pos.start + 1)
            }
            ctx.addLeft(start, before) +
              ctx.addRight(end, after)
          }
      }
      .flatten
      .asPatch
  }
}
