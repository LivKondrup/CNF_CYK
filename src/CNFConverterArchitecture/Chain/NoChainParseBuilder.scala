package CNFConverterArchitecture.Chain

import GrammarArchitecture.{NonTerminal, Rule}

class NoChainParseBuilder extends ChainParseBuilder {
  override def newDerivation(from: NonTerminal, to: NonTerminal, rule: Rule): Unit = {

  }
}
