package ParseTreeArchitecture

import GrammarArchitecture.{NonTerminal, Terminal}

import scala.collection.mutable.ListBuffer

sealed abstract class ParseTree

case class ParseTreeLeaf(name: Terminal) extends ParseTree

case class ParseTreeNode(name:NonTerminal, children: ListBuffer[ParseTree]) extends ParseTree
