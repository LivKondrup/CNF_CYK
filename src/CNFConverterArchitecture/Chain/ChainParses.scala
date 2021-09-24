package CNFConverterArchitecture.Chain

import GrammarArchitecture.{NonTerminal, Rule}
import ParseTreeArchitecture.{ParseTree, ParseTreeLeaf, ParseTreeNode}

import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer}

class ChainParses extends ChainParseBuilder {
  private val chainParses = HashMap[(NonTerminal, NonTerminal), ParseTree]()

  private def addChildToBottomOfTree(parseTree: ParseTree, child: NonTerminal): ParseTree = {
    parseTree match {
      case ParseTreeNode(name, children) => ParseTreeNode(name, ListBuffer(addChildToBottomOfTree(children.head, child)))  // These parses are all chain parses, so every node will have at most one child
      case ParseTreeLeaf(name) => ParseTreeNode(NonTerminal(name.getName()), ListBuffer(ParseTreeLeaf(child)))    // The leaf is always a nonTerminal here because we only look at parses from nonterminal to nonterminal
    }
  }

  def newDerivation(from: NonTerminal, to: NonTerminal, rule: Rule): Unit = {
    var newParseTree: ParseTree = null
    if (rule.getLeft() == from && rule.getRight().head == to){    // When the method is called, the rule should always be a chain rule otherwise it makes no sense (since there are no lambdas left)
      newParseTree = ParseTreeNode(from, ListBuffer(ParseTreeLeaf(to)))
    } else {
      val existingParseTreeToHere = chainParses((from, rule.getLeft()))
      newParseTree = addChildToBottomOfTree(existingParseTreeToHere, to)
    }

    chainParses((from, to)) = newParseTree
  }

  def getParses(): HashMap[(NonTerminal, NonTerminal), ParseTree] = {
    chainParses
  }
}
