import GrammarArchitecture.{NonTerminal, RuleElement, Terminal}

sealed abstract class DerivationTree

case class Leaf(name: Terminal) extends DerivationTree

case class DerivationTreeNode(name:NonTerminal, children: Set[RuleElement]) extends DerivationTree
