package ParseTreeArchitecture

import GrammarArchitecture.{NonTerminal, RuleElement, Terminal}

import scala.collection.mutable.ListBuffer

sealed abstract class ParseTree

case class ParseTreeLeaf(name: RuleElement) extends ParseTree

case class ParseTreeNode(name:NonTerminal, children: ListBuffer[ParseTree]) extends ParseTree
