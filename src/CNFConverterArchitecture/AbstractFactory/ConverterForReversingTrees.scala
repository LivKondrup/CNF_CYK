package CNFConverterArchitecture.AbstractFactory
import CNFConverterArchitecture.Chain.{ChainParseBuilder, ChainParses}
import CNFConverterArchitecture.Lambda.{LambdaParseBuilder, LambdaParses}
import GrammarArchitecture.Grammar
import HistoryTreeArchitecture.{HistoryTreeBuilder, HistoryBuilder}

class ConverterForReversingTrees() extends CNFConverterFactory {
  override def createLambdaParseBuilder(): LambdaParseBuilder = new LambdaParses()

  override def createChainParseBuilder(): ChainParseBuilder = new ChainParses()

  override def createHistoryTreeBuilder(): HistoryBuilder = new HistoryTreeBuilder()
}
