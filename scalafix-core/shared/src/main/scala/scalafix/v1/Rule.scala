package scalafix.v1

import metaconfig.Conf
import metaconfig.Configured
import scalafix.Patch
import scalafix.rule.RuleName

trait RuleFactory {
  def newRule(conf: Conf): Configured[Rule]
}

abstract class Rule(name: RuleName)

abstract class SyntacticRule(name: RuleName) extends Rule(name) {
  def fix(doc: Doc): Patch
}

abstract class SemanticRule(name: RuleName) extends Rule(name) {
  def fix(doc: SemanticDoc): Patch
}
