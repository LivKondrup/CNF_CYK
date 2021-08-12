package HistoryTreeArchitecture

import GrammarArchitecture.Rule

trait RuleUpdatingBuilder {
  def ruleUpdated(oldRule: Rule, newRule: Rule, step: Int): Unit
}
