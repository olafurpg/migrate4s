package scalafix
package rewrite
import scala.meta._

object ScalafixRewrites {
  val syntax: List[Rewrite] = List(
    ProcedureSyntax,
    VolatileLazyVal,
    RemoveXmlLiterals,
    ExplicitUnit,
    NoValInForComprehension,
    DottyVarArgPattern
  )
  def semantic(mirror: Database): List[Rewrite] = List(
    ExpandSugars(mirror),
    ExplicitReturnTypes(mirror),
    RemoveUnusedImports(mirror),
    NoAutoTupling(mirror)
  )
  def all(mirror: Database): List[Rewrite] =
    syntax ++ semantic(mirror)
  def name2rewrite(mirror: Database): Map[String, Rewrite] =
    all(mirror).map(x => x.name -> x).toMap
  lazy val syntaxName2rewrite: Map[String, Rewrite] =
    syntax.map(x => x.name -> x).toMap
  val emptyDatabase = Database(Nil)
  lazy val syntacticNames: List[String] = syntaxName2rewrite.keys.toList
  lazy val semanticNames: List[String] = semantic(emptyDatabase).map(_.name)
  def allNames: List[String] = syntaxName2rewrite.keys.toList ++ semanticNames
}
