package CNFConverterArchitecture.Lambda

import GrammarArchitecture.{Lambda, NonTerminal, Rule}
import ParseTreeArchitecture.{ParseTree, ParseTreeLeaf, ParseTreeNode}

import scala.collection.mutable.{HashMap, ListBuffer}

class LambdaParses extends LambdaParseBuilder {
  private var lambdaParses: HashMap[NonTerminal, ParseTree] = new HashMap()

  // Saves the variables that can parse directly to lambda
  override def initializeParseTrees(nullableVariables: ListBuffer[NonTerminal]): Unit = {
    for(variable <- nullableVariables){
      val treeOfVariableToLambda = ParseTreeNode(variable, ListBuffer(ParseTreeLeaf(Lambda())))   // variable -> Lambda
      lambdaParses += (variable -> treeOfVariableToLambda)
    }
  }

  // Assumes the given rule is nullable from the other nullables in "lambdaParses"
  override def updateLambdaParses(rule: Rule): Unit ={
    val childTrees:ListBuffer[ParseTree] = ListBuffer()
    for(elem <- rule.getRight()){
      childTrees += lambdaParses(NonTerminal(elem.getName()))     // The elem will always be a NonTerminal, otherwise the rule would not be nullable
    }
    val nonTerminal = rule.getLeft()
    val parseTree = ParseTreeNode(nonTerminal, childTrees)
    lambdaParses += (nonTerminal -> parseTree)
  }

  override def getLambdaParses(nonTerminal: NonTerminal): ParseTree ={
    lambdaParses(nonTerminal)
  }


}
