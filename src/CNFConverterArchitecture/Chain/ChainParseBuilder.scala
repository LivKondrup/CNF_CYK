package CNFConverterArchitecture.Chain

import GrammarArchitecture.{NonTerminal, Rule}

trait ChainParseBuilder {
  def newDerivation(from: NonTerminal, to: NonTerminal, rule: Rule): Unit
}
