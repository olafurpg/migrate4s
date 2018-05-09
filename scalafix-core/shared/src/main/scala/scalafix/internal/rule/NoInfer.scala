package scalafix.internal.rule

import scala.meta._
import scalafix.Patch
import scalafix.internal.config.NoInferConfig
import scalafix.rule.SemanticRule
import scalafix.util.SemanticdbIndex
import scalafix.v1.SemanticDoc

final case class NoInfer(index: SemanticdbIndex, config: NoInferConfig)
    extends SemanticRule(index, "NoInfer")
    with Product {

  override def fix(implicit doc: SemanticDoc): Patch =
    Patch.empty
}

case object NoInfer {

  def badSymbolNames: List[String] = NoInferConfig.badSymbols.collect {
    case Symbol.Global(_, signature) => signature.name
  }
}
