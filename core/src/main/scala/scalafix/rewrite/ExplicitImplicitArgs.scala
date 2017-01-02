package scalafix.rewrite

import scala.collection.mutable
import scala.util.matching.Regex
import scala.{meta => m}
import scalafix.util.Patch
import scalafix.util.logger
import scalafix.util.syntax._

// Inserts inferred implicit argument lists.
case object ExplicitImplicitArgs extends Rewrite {
  val EVIDENCE = "evidence$"
  val EvidenceParam: Regex = "evidence\\$(\\d+)".r
  override def rewrite(ast: m.Tree, ctx: RewriteCtx): Seq[Patch] = {
    import scala.meta._
    val semantic = getSemanticApi(ctx)
    import semantic._
    val taken = mutable.Set.empty[Token]
    ast.collect {
      case original: Term.Apply =>
        DTerm
          .unapply(original)
          .collect {
            case desugared: Term.Apply =>
              val desugaredArgss = desugared.argss
              val originalArgss = original.argss
              def desugaredHasImplicitArgList =
                desugaredArgss.length == originalArgss.length + 1
              def someNonEvidence =
                desugaredArgss.head.exists(!_.syntax.contains(EVIDENCE))
              if (desugaredHasImplicitArgList &&
                  someNonEvidence &&
                  !taken(original.tokens.last)) {
                val tok = original.tokens.last
                taken += tok
                val implicitArgs = desugaredArgss.head.map {
                  case Term.Name(EvidenceParam(id)) => q"implicitly"
                  case t => t
                }
//                logger.elem(original -> desugaredArgss.head)
                Seq(
                  Patch(tok, tok, s"$tok(${implicitArgs.mkString(", ")})")
                )
              } else Nil
          }
          .toSeq
          .flatten
    }.flatten
  }
}
