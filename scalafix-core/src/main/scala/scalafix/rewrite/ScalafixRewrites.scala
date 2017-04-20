package scalafix
package rewrite

object ScalafixRewrites {
  val syntax: List[Rewrite] = List(
    ProcedureSyntax,
    VolatileLazyVal
  )
  def semantic(mirror: ScalafixMirror): List[Rewrite] = List(
    ScalaJsRewrites.DemandJSGlobal(mirror),
    ExplicitImplicit(mirror),
    Scalameta17(mirror),
    Xor2Either(mirror)
  )
  val all: List[ScalafixRewrite] = syntax ++ semantic
  val default: List[ScalafixRewrite] =
    all.filterNot(Set(VolatileLazyVal, Xor2Either))
  val name2rewrite: Map[String, ScalafixRewrite] =
    all.map(x => x.name -> x).toMap

}
