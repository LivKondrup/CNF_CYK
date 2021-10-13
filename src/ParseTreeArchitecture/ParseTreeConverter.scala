package ParseTreeArchitecture

import CNFConverterArchitecture.Chain.ChainParses
import CNFConverterArchitecture.Lambda.{LambdaParseBuilder, LambdaParses}
import GrammarArchitecture.{Lambda, NonTerminal, Rule, RuleElement, Terminal}
import CNFConverterArchitecture.HistoryTreeArchitecture.{HistoryTree, HistoryTreeBuilder, HistoryTreeLeaf, HistoryTreeNode}

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

object ParseTreeConverter {

  def reverseTreeToOriginalGrammar(parseTree: ParseTree, historyTreeBuilder: HistoryTreeBuilder, lambdaParses: LambdaParses, chainParses: ChainParses): ParseTree = {
    var reversedParseTree = reverseRenaming(parseTree, historyTreeBuilder)
    reversedParseTree = reverseChains(reversedParseTree, historyTreeBuilder, chainParses)
    reversedParseTree = reverseLambda(reversedParseTree, historyTreeBuilder, lambdaParses)
    reversedParseTree
  }

  def reverseLambda(tree: ParseTree, historyTreeBuilder: HistoryTreeBuilder, lambdaBuilder: LambdaParseBuilder): ParseTree = {
    reverseLambdaOfSubTrees(tree, historyTreeBuilder, lambdaBuilder).head
  }

  private def reverseLambdaOfSubTrees(tree: ParseTree, historyTreeBuilder: HistoryTreeBuilder, lambdaBuilder: LambdaParseBuilder): ListBuffer[ParseTree] = {
    tree match {
      case ParseTreeNode(name, children) =>
        var childTrees: ListBuffer[ParseTree] = ListBuffer()
        for(child <- children){
          childTrees ++= reverseLambdaOfSubTrees(child, historyTreeBuilder, lambdaBuilder)
        }
        var currentRule = getCurrentRule(name, children)

        val prevRule = getPreviousRule(historyTreeBuilder, name, children, 1)

        val currentRuleIsInAHistoryTree = historyTreeBuilder.findTreeWithRule(currentRule, 1) != HistoryTreeLeaf
        val prevRuleExists = prevRule != null
        if(!currentRuleIsInAHistoryTree){   // The previous rule is not an original rule (or the top layer of a lambda rule), so just pass the child-trees on the the next layer of the tree
          return childTrees
        }

        currentRule = getCurrentRule(name, childTrees)

        if(!prevRuleExists || currentRule.getRight() == prevRule.getRight()){    // the rules are equal, so there are no lambda parses missing from the tree or it is just a normal rule that was not affected in this step
          return ListBuffer(ParseTreeNode(name, childTrees))
        }

        var newChildTrees: ListBuffer[ParseTree] = ListBuffer()
        var lastFoundIndex: Int = -1
        for(elem <- prevRule.getRight()){
          val currentRuleRightSide = currentRule.getRight()
          breakable{
            for(i <- lastFoundIndex + 1 until currentRuleRightSide.size){    // Only go through the ones that have not already been found
              if(elem.isInstanceOf[Terminal]){
                newChildTrees += ParseTreeLeaf(Terminal(elem.getName()))
                lastFoundIndex += 1
                break   // go to next element if the current is a terminal - this is the end of this tree
              }
              if(currentRuleRightSide(i) == elem){
                lastFoundIndex += 1 // increase counter
                newChildTrees += childTrees(i)  //add the tree from childTrees to the new child-trees list
                break // the elem is already included in current tree, move on to next elem
              }
              // build the lambda parse for this elem and include in tree
              val lambdaParse = lambdaBuilder.getLambdaParses(NonTerminal(elem.getName()))
              newChildTrees += lambdaParse
            }
          }
        }
        ListBuffer(ParseTreeNode(name, newChildTrees))

      case ParseTreeLeaf(name) => ListBuffer(ParseTreeLeaf(name))
    }
  }

  def reverseLambda2(tree: ParseTree, builder: HistoryTreeBuilder, lambdaParses: LambdaParseBuilder): ParseTree = {
    tree match {
      case ParseTreeNode(nonTerm, children) =>
        val currentRule = getCurrentRule(nonTerm, children)
        val prevRule = getPreviousRule(builder, nonTerm, children, 1)

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

  def reverseChains(tree: ParseTree, builder: HistoryTreeBuilder, parses: ChainParses): ParseTree ={
    reverseChainsOfSubTrees(tree, builder, parses).head
  }

  def reverseChainsOfSubTrees(tree: ParseTree, builder: HistoryTreeBuilder, chainParses: ChainParses): ListBuffer[ParseTree] = {
    tree match {
      case ParseTreeLeaf(name) => ListBuffer(ParseTreeLeaf(name))
      case ParseTreeNode(name, children) =>
        val prevRule = getPreviousRule(builder, name, children, 2)
        val childTrees: ListBuffer[ParseTree] = ListBuffer()

        for (child <- children) {
          val newChild = reverseChainsOfSubTrees(child, builder, chainParses)
          childTrees ++= newChild
        }

        if(prevRule == null){ // TODO: should be something else maybe??
          return ListBuffer(ParseTreeNode(name, childTrees))
        }

        if(prevRule.getRight().head == Lambda() && prevRule.getRight().length == 2){    // A chain rule with lambda in front (the way derivations are represented)
          var chainTree = chainParses.getParses()((prevRule.getLeft(), NonTerminal(prevRule.getRight()(1).getName())))  // prevRule is chainRule derivation, so right side is nonTerminal with lambda in front
          // add chainTree as parent to childTrees
          return ListBuffer(addChildrenToBottomOfChainTree(chainTree, childTrees))
        }

        childTrees
    }
  }

  // assumes that tree is a chain tree
  def addChildrenToBottomOfChainTree(tree: ParseTree, children: ListBuffer[ParseTree]): ParseTree = {
    tree match {
      case ParseTreeNode(name, nodeChildren) => ParseTreeNode(name, ListBuffer(addChildrenToBottomOfChainTree(nodeChildren.head, children)))  // There is only one child in nodeChildren since the tree is a chain tree
      case ParseTreeLeaf(name) => ParseTreeNode(NonTerminal(name.getName()), children)  // The input tree is a chain tree of only nonterminals
    }
  }

  def reverseRenaming(parseTree: ParseTree, historyTreeBuilder: HistoryTreeBuilder): ParseTree = {
    val childTrees = reverseRenamingOfSubTrees(parseTree, historyTreeBuilder)
    childTrees.head
  }

  private def reverseRenamingOfSubTrees(parseTree: ParseTree, historyTreeBuilder: HistoryTreeBuilder): ListBuffer[ParseTree] = {
    parseTree match {
      case ParseTreeNode(name, children) =>
        // Recursively reverse the child trees
        val childTrees: ListBuffer[ParseTree] = ListBuffer()
        for (child <- children) {
          val newChild = reverseRenamingOfSubTrees(child, historyTreeBuilder)
          childTrees ++= newChild
        }

        // If the previous rule exist (meaning it wasn't only invented for renaming)
        // Combine the current root with the converted child trees
        val currentRule = getCurrentRule(name, children)
        val currentRuleExistsInHistoryTrees = historyTreeBuilder.findTreeWithRule(currentRule, 3) != HistoryTreeLeaf
        if (currentRuleExistsInHistoryTrees) {
          ListBuffer(ParseTreeNode(name, childTrees))
        } else { // The rule was invented for renaming and the list of reversed subtrees, should just be passed on to the parent (this part is where layers are decreased)
          childTrees
        }
      case ParseTreeLeaf(term) => ListBuffer(ParseTreeLeaf(term))
    }
  }

  private def getPreviousRule(historyTreeBuilder: HistoryTreeBuilder, name: NonTerminal, children: ListBuffer[ParseTree], step:Int) = {
    val rule = getCurrentRule(name, children)

    // Find the rule previous to this rule in the history trees
    val prevRule = historyTreeBuilder.getPreviousRule(rule, step)
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

  /*private def treeHasRulesFromStep(tree: ParseTree, step: Int, historyTreeBuilder: HistoryTreeBuilder): Boolean = {
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
  }*/

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
