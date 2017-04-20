package scalafix

import scala.meta._
import scala.util.control.NonFatal
import scalafix.config.ScalafixConfig

object Scalafix {
  def syntaxFix(input: Input, config: ScalafixConfig): Fixed =
    config.dialect(input).parse[Source] match {
      case parsers.Parsed.Success(ast) =>
        fix(RewriteCtx.syntactic(ast, config))
      case parsers.Parsed.Error(pos, msg, details) =>
        Fixed.Failed(Failure.ParseError(pos, msg, details))
    }

  def fix[T](ctx: RewriteCtx): Fixed =
    try {
      val combinedRewrite =
        ctx.config.rewrites.foldLeft(Rewrite.empty)(_ andThen _)
      Fixed.Success(
        combinedRewrite
          .rewrite(ctx)
          .applied
      )
    } catch {
      case NonFatal(e) =>
        Fixed.Failed(Failure.Unexpected(e))
    }

}
