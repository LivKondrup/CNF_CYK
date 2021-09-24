package CNFConverterArchitecture.AbstractFactory
import CNFConverterArchitecture.Chain.{ChainParseBuilder, NoChainParseBuilder}
import CNFConverterArchitecture.Lambda.{LambdaParseBuilder, NoLambdaParseBuilder}
import CNFConverterArchitecture.HistoryTreeArchitecture.{HistoryTreeBuilder, NoUpdatingBuilder, HistoryBuilder}

class SimpleConverter extends CNFConverterFactory {
  override def createLambdaParseBuilder(): LambdaParseBuilder = new NoLambdaParseBuilder()

  override def createChainParseBuilder(): ChainParseBuilder = new NoChainParseBuilder

  override def createHistoryTreeBuilder(): HistoryBuilder = new NoUpdatingBuilder
}
