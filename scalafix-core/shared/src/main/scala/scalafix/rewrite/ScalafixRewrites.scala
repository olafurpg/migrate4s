package scalafix
package rewrite

import scalafix.internal.rewrite._

object ScalafixRewrites {
  val syntax: List[Rewrite] = List(
    ProcedureSyntax,
    DottyVolatileLazyVal,
    RemoveXmlLiterals,
    ExplicitUnit,
    NoValInForComprehension,
    DottyKeywords,
    DottyVarArgPattern
  )
  def semantic(semanticCtx: SemanticCtx): List[Rewrite] = List(
    Sbt1(semanticCtx),
    ExplicitReturnTypes(semanticCtx),
    RemoveUnusedImports(semanticCtx),
    NoAutoApply(mirror),
    NoAutoTupling(semanticCtx)
  )
  def all(semanticCtx: SemanticCtx): List[Rewrite] =
    syntax ++ semantic(semanticCtx)
  def name2rewrite(semanticCtx: SemanticCtx): Map[String, Rewrite] =
    all(semanticCtx).flatMap(x => x.names.map(_ -> x)).toMap
  lazy val syntaxName2rewrite: Map[String, Rewrite] =
    syntax.flatMap(x => x.names.map(_ -> x)).toMap
  val emptyDatabase = SemanticCtx(Nil)
  lazy val syntacticNames: List[String] = syntaxName2rewrite.keys.toList
  lazy val semanticNames: List[String] =
    semantic(emptyDatabase).flatMap(_.names)
  def allNames: List[String] = syntaxName2rewrite.keys.toList ++ semanticNames
}
