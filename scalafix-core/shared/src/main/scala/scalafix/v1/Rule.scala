package scalafix.v1

import metaconfig.Conf
import metaconfig.Configured
import scalafix.Patch
import scalafix.rule.RuleName

abstract class Rule(ruleName: RuleName) {

  final def name: RuleName = ruleName

  def withConfig(conf: Conf): Configured[Rule] = Configured.ok(this)

  @deprecated("Use withConfig instead", "0.6.0")
  def init(conf: Conf): Configured[Rule] =
    throw new UnsupportedOperationException("use withConfig instead")

}

abstract class SyntacticRule(name: RuleName) extends Rule(name) {
  def fix(implicit doc: Doc): Patch = Patch.empty
}

abstract class SemanticRule(name: RuleName) extends Rule(name) {
  def fix(implicit doc: SemanticDoc): Patch = Patch.empty
}
