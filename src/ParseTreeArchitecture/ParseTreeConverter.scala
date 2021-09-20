package ParseTreeArchitecture

import CNFConverterArchitecture.Lambda.{LambdaParseBuilder, LambdaParses}
import GrammarArchitecture.{NonTerminal, Rule, RuleElement}
import HistoryTreeArchitecture.{HistoryTree, HistoryTreeBuilder, HistoryTreeLeaf, HistoryTreeNode}

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

object ParseTreeConverter {

  def reverseTreeToOriginalGrammar(parseTree: ParseTree, historyTreeBuilder: HistoryTreeBuilder, lambdaParses: LambdaParses): ParseTree = {
    var reversedParseTree = reverseRenaming(parseTree, historyTreeBuilder)
    reversedParseTree = reverseChains(parseTree, historyTreeBuilder)
    reversedParseTree = reverseLambda(parseTree, historyTreeBuilder, lambdaParses)
    reversedParseTree
  }

  def reverseLambda(tree: ParseTree, builder: HistoryTreeBuilder, lambdaParses: LambdaParseBuilder): ParseTree = {
    tree match {
      case ParseTreeNode(nonTerm, children) =>
        val currentRule = getCurrentRule(nonTerm, children)
        val prevRule = getPreviousRule(builder, nonTerm, children)

        var childTrees: ListBuffer[ParseTree] = ListBuffer()
        var alreadyBuild = 0

        if (currentRule.getRight().length < prevRule.getRight().length){
          // Find the new variables
          for (i <- currentRule.getRight().indices){
            breakable {
              for (j <- alreadyBuild until prevRule.getRight().length) {
                if (!(prevRule.getRight()(j) == currentRule.getRight()(i))) {
                  // build lambda parse
                  childTrees += lambdaParses.getLambdaParses(NonTerminal(prevRule.getRight()(j).getName()))
                  alreadyBuild += 1
                } else {
                  alreadyBuild += 1
                  break
                }
              }
            }
            // recursively build childTrees of i
            childTrees += reverseLambda(children(i), builder, lambdaParses)
          }
          // building the rest of the tree
          for (j <- alreadyBuild until prevRule.getRight().length){
            // build the remaining lambda parses
            childTrees += lambdaParses.getLambdaParses(NonTerminal(prevRule.getRight()(j).getName()))
          }
        } else {    // If there is no extra variable in previous rule
          for(child <- children){
            childTrees += reverseLambda(child, builder, lambdaParses)
          }
        }
        ParseTreeNode(nonTerm, childTrees)

      case ParseTreeLeaf(_) => tree
    }
  }

  /*def reverseChains(tree: ParseTree, builder: HistoryTreeBuilder): ParseTree = {
    tree match {
      case ParseTreeLeaf(name) =>
      case ParseTreeNode(name, children) =>
    }
  }*/

  def reverseChains(parseTree: ParseTree, historyTreeBuilder: HistoryTreeBuilder): ParseTree = {

    var treeWithChainRules = reverseChainsOfSubTrees(parseTree, historyTreeBuilder)

    var treeHasRulesGeneratedInStepTwo = treeHasRulesFromStep(treeWithChainRules.head, 2, historyTreeBuilder)

    while (treeHasRulesGeneratedInStepTwo){
      treeWithChainRules = reverseChainsOfSubTrees(treeWithChainRules.head, historyTreeBuilder)
      treeHasRulesGeneratedInStepTwo = treeHasRulesFromStep(treeWithChainRules.head, 2, historyTreeBuilder)
    }

    treeWithChainRules.head
  }

  private def reverseChainsOfSubTrees(parseTree: ParseTree, historyTreeBuilder: HistoryTreeBuilder): ListBuffer[ParseTree] = {
    parseTree match {
      case ParseTreeNode(name, children) =>
        // Build the rule at the root of the current tree
        val prevRule: Rule = getPreviousRule(historyTreeBuilder, name, children)

        // build child trees
        val childTrees: ListBuffer[ParseTree] = ListBuffer()
        for (child <- children) {
          val newChild = reverseChainsOfSubTrees(child, historyTreeBuilder)
          childTrees ++= newChild
        }

        if (prevRule.isChainRule()) {
          // Since the rule is a chain rule the right side will only have one element, which is a NonTerminal
          val newSecondLayer = NonTerminal(prevRule.getRight().head.getName())
          val newRoot = prevRule.getLeft()
          ListBuffer(ParseTreeNode(newRoot, ListBuffer(ParseTreeNode(newSecondLayer, childTrees))))
        } else {
          ListBuffer(parseTree)
        }
      case ParseTreeLeaf(term) => ListBuffer(ParseTreeLeaf(term))
    }
  }

  // TODO: does not check if reversal is in correct step
  def reverseRenaming(parseTree: ParseTree, historyTreeBuilder: HistoryTreeBuilder): ParseTree = {
    val childTrees = reverseRenamingOfSubTrees(parseTree, historyTreeBuilder)
    childTrees.head
  }

  private def reverseRenamingOfSubTrees(parseTree: ParseTree, historyTreeBuilder: HistoryTreeBuilder): ListBuffer[ParseTree] = {
    parseTree match {
      case ParseTreeNode(name, children) =>
        // Build the rule at the root of the current tree
        val prevRule: Rule = getPreviousRule(historyTreeBuilder, name, children)

        // Recursively reverse the child trees
        val childTrees: ListBuffer[ParseTree] = ListBuffer()
        for (child <- children) {
          val newChild = reverseRenamingOfSubTrees(child, historyTreeBuilder)
          childTrees ++= newChild
        }

        // If the previous rule exist (meaning it wasn't only invented for renaming)
        // Combine the current root with the converted child trees
        if (prevRule != null) {
          ListBuffer(ParseTreeNode(name, childTrees))
        } else { // The rule was invented for renaming and the list of reversed subtrees, should just be passed on to the parent (this part is where layers are decreased)
          childTrees
        }
      case ParseTreeLeaf(term) => ListBuffer(ParseTreeLeaf(term))
    }
  }

  private def getPreviousRule(historyTreeBuilder: HistoryTreeBuilder, name: NonTerminal, children: ListBuffer[ParseTree]) = {
    val rule = getCurrentRule(name, children)

    // Find the rule previous to this rule in the history trees
    val prevRule = historyTreeBuilder.getPreviousRule(rule)
    prevRule
  }

  private def getCurrentRule(name: NonTerminal, children: ListBuffer[ParseTree]): Rule = {
    var rightSide: ListBuffer[RuleElement] = ListBuffer()
    for (child <- children) {
      child match {
        case ParseTreeNode(name, _) => rightSide += name
        case ParseTreeLeaf(term) => rightSide += term
      }
    }
    new Rule(name, rightSide)
  }

  private def treeHasRulesFromStep(tree: ParseTree, step: Int, historyTreeBuilder: HistoryTreeBuilder): Boolean = {
    tree match {
      case ParseTreeNode(nonTerminal, children) =>
        val rightSide: ListBuffer[RuleElement] = children.map{
          case ParseTreeNode(nonTerm, _) => nonTerm
          case ParseTreeLeaf(term) => term
        }
        val currentRule = new Rule(nonTerminal, rightSide)
        val historyTreeWithRule = historyTreeBuilder.findTreeWithRule(currentRule)
        val stepOfPrevRuleInHistoryTree = findStepOfRuleInTree(currentRule, historyTreeWithRule)
        if (stepOfPrevRuleInHistoryTree == step){
          return true
        }
        for(child <- children){
          if (treeHasRulesFromStep(child, step, historyTreeBuilder)){
            return true
          }
        }
        return false
      case ParseTreeLeaf(term) => return false
    }
    false
  }

  private def findStepOfRuleInTree(rule: Rule, historyTree: HistoryTree): Int ={
    historyTree match {
      case HistoryTreeNode(currentRule, children, step) =>
        if(rule == currentRule){
          return step
        }
        for(child <- children){
          val stepInChild = findStepOfRuleInTree(rule, child)
          if(stepInChild != -1){
            return stepInChild
          }
        }
        -1
      case HistoryTreeLeaf => -1
    }
  }
}
