package HistoryTreeArchitecture

import GrammarArchitecture.{Grammar, Rule}

class NoUpdatingBuilder extends HistoryBuilder {
  override def ruleUpdated(oldRule: Rule, newRule: Rule, step: Int): Unit = {

  }

  override def init(grammar: Grammar): Unit = {

  }
}
