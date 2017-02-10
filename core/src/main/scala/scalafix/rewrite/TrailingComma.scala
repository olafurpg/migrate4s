package scalafix.rewrite

import scala.collection.immutable.Seq
import scala.meta.Tree
import scala.meta.tokens.Token.KwImplicit
import scala.meta.tokens.Token.Space
import scalafix.util.CloseDelim
import scalafix.util.ConflictStrategy
import scalafix.util.LineEnd
import scalafix.util.OpenDelim
import scalafix.util.Patch
import scalafix.util.TokenPatch
import scalafix.util.TreeExtractors.LastArgument

import org.scalameta.logger

case object TrailingComma extends Rewrite {
  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] =
    code.collect {
      case LastArgument(args) =>
        args.flatMap { arg =>
          val last = arg.tokens.last
          val next = ctx.tokenList.next(last)
          val isMissingTrailingComma = (for {
            close <- ctx.tokenList.find(next)(!_.is[Space])
            if next.is[LineEnd]
            if close.is[CloseDelim]
            open <- ctx.matchingParentheses.get(close)
            if open.is[OpenDelim]
            afterOpen = ctx.tokenList.next(open)
            if afterOpen.is[LineEnd] || afterOpen.is[KwImplicit]
          } yield true).getOrElse(false)

          if (isMissingTrailingComma)
            Some(
              TokenPatch.Add(
                last,
                addLeft = "",
                addRight = ",",
                conflictStrategy = ConflictStrategy.MergeIfEqual
              ))
          else None
        }
    }.flatten
}
