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
//    logger.elem(ast)
    ast.collect {
      case original: Term.Apply =>
        DTerm
          .unapply(original)
          .toSeq
          .flatMap {
            case desugared: Term.Apply =>
              val desugaredArgss = desugared.argss
              val originalArgss = original.argss
              logger.elem(desugared)
              if (desugaredArgss.length == originalArgss.length + 1) {
                val tok = original.tokens.last
                logger.elem(desugaredArgss.head)
                Seq(
                  Patch(tok,
                        tok,
                        s"$tok(${desugaredArgss.head.mkString(", ")})")
                )
              } else Nil
          }
    }.flatten
  }
}
