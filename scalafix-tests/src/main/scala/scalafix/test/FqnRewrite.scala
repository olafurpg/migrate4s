package banana.rewrite

import scala.meta._, contrib._
import scalafix._

import org.scalameta.logger

case class FqnRewrite(implicit mirror: Mirror)
    extends SemanticRewrite(mirror) {
  override def rewrite(ctx: RewriteCtx): Patch =
    ctx.addGlobalImport(importer"scala.meta._")
}

case object FqnRewrite2 extends Rewrite {
  override def rewrite(ctx: RewriteCtx): Patch =
    ctx.tree.collectFirst {
      case n: Name => ctx.rename(n, Term.Name(n.value + "2"))
    }.asPatch
}

object LambdaRewrites {
  val syntax = Rewrite.syntactic { ctx =>
    ctx.addLeft(ctx.tokens.head, "// comment \n")
  }
}
