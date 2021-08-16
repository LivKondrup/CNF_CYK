import GrammarArchitecture.{NonTerminal, RuleElement, Terminal}

import scala.collection.mutable.ListBuffer

sealed abstract class DerivationTree

case class Leaf(name: Terminal) extends DerivationTree

case class DerivationTreeNode(name:NonTerminal, children: ListBuffer[DerivationTree]) extends DerivationTree
