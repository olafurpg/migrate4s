package scalafix.rewrite

import scala.{meta => m}
import scalafix.util.Patch
import scalafix.util.logger
import scalafix.util.syntax._

// Inserts inferred implicit argument lists.
case object ExplicitImplicitArgs extends Rewrite {
  override def rewrite(ast: m.Tree, ctx: RewriteCtx): Seq[Patch] = {
    import scala.meta._
    val semantic = getSemanticApi(ctx)
    import semantic._

    implicit class ApplyPimp(t: Term.Apply) {
      def argss: Seq[Seq[Term.Arg]] = t.fun match {
        case tt: Term.Apply => t.args +: tt.argss
        case _ => Seq(t.args)
      }
    }
    ast.collect {
      case (original: Term.Apply) & DTerm(desugared: Term.Apply) =>
        val desugaredArgss: Seq[Seq[Term.Arg]] = desugared.argss
        val originalArgss: Seq[Seq[Term.Arg]] = original.argss
        if (desugaredArgss.length == originalArgss.length + 1) {
          val tok = original.tokens.last
          logger.elem(desugaredArgss.head)
          Seq(
            Patch(tok, tok, s"$tok(${desugaredArgss.head.mkString(", ")})")
          )
        } else Nil
    }.flatten
  }
}
