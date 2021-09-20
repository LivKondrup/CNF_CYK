package CNFConverterArchitecture.AbstractFactory

import CNFConverterArchitecture.Chain.ChainParseBuilder
import CNFConverterArchitecture.Lambda.LambdaParseBuilder
import HistoryTreeArchitecture.{HistoryTreeBuilder, HistoryBuilder}

trait CNFConverterFactory {
  def createLambdaParseBuilder(): LambdaParseBuilder
  def createChainParseBuilder(): ChainParseBuilder
  def createHistoryTreeBuilder(): HistoryBuilder

}
