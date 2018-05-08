package banana.rule

import scala.meta._
import scala.meta.contrib._
import scalafix._

case object FqnRule extends SemanticRule("FqnRule") {
  override def fix(ctx: RuleCtx): Patch =
    ctx.addGlobalImport(importer"scala.collection.immutable")
}

case object FqnRule2 extends Rule("FqnRule2") {
  override def fix(ctx: RuleCtx): Patch =
    ctx.tree.collectFirst {
      case n: Name => ctx.replaceTree(n, n.value + "2")
    }.asPatch
}

object LambdaRules {
  val syntax: Rule = Rule.syntactic("syntax") { ctx =>
    ctx.addLeft(ctx.tokens.head, "// comment\n")
  }

  val semantic = Rule.semantic("semantic") { implicit index => ctx =>
    ctx.addGlobalImport(importer"scala.collection.mutable")
  }

}

case object PatchTokenWithEmptyRange extends Rule("PatchTokenWithEmptyRange") {
  override def fix(ctx: RuleCtx): Patch = {
    ctx.tokens.collect {
      case tok @ Token.Interpolation.SpliceEnd() =>
        ctx.addRight(tok, "a")
      case tok @ Token.Xml.SpliceEnd() =>
        ctx.addRight(tok, "a")
    }
  }.asPatch
}
