package CNFConverterArchitecture.AbstractFactory
import CNFConverterArchitecture.Chain.{ChainParseBuilder, ChainParses}
import CNFConverterArchitecture.Lambda.{LambdaParseBuilder, LambdaParses}
import GrammarArchitecture.Grammar
import CNFConverterArchitecture.HistoryTreeArchitecture.{HistoryTreeBuilder, HistoryBuilder}

class ConverterForReversingTrees() extends CNFConverterFactory {
  private var lambdaParses:LambdaParses = _
  private var chainParses:ChainParses = _
  private var builder:HistoryTreeBuilder = _

  override def createLambdaParseBuilder(): LambdaParseBuilder = {
    lambdaParses = new LambdaParses()
    lambdaParses
  }

  override def createChainParseBuilder(): ChainParseBuilder = {
    chainParses = new ChainParses()
    chainParses
  }

  override def createHistoryTreeBuilder(): HistoryBuilder = {
    builder = new HistoryTreeBuilder()
    builder
  }

  def getLambdaParses: LambdaParses = {
    lambdaParses
  }

  def getChainParses: ChainParses = {
    chainParses
  }

  def getHistoryTreeBuilder: HistoryTreeBuilder = {
    builder
  }

}
