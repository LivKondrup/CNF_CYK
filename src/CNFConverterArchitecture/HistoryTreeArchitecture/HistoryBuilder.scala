package CNFConverterArchitecture.HistoryTreeArchitecture

import GrammarArchitecture.{Grammar, Rule}

trait HistoryBuilder {
  def init(grammar: Grammar, step: Int): Unit
  def ruleUpdated(oldRule: Rule, newRule: Rule, step: Int): Unit
}
