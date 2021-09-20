package CNFConverterArchitecture.Lambda

import GrammarArchitecture.{Lambda, NonTerminal, Rule}
import ParseTreeArchitecture.ParseTree

import scala.collection.mutable.{HashMap, ListBuffer}

trait LambdaParseBuilder {
  def initializeParseTrees(nullableVariables: ListBuffer[NonTerminal]): Unit
  def updateLambdaParses(rule: Rule): Unit
  def getLambdaParses(nonTerminal: NonTerminal): ParseTree
}



