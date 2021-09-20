package CNFConverterArchitecture.Lambda
import GrammarArchitecture.{NonTerminal, Rule}
import ParseTreeArchitecture.ParseTree

import scala.collection.mutable.ListBuffer

class NoLambdaParseBuilder  extends LambdaParseBuilder {
  override def initializeParseTrees(nullableVariables: ListBuffer[NonTerminal]): Unit = {

  }

  override def updateLambdaParses(rule: Rule): Unit = {

  }

  override def getLambdaParses(nonTerminal: NonTerminal): ParseTree = {
    null
  }
}
