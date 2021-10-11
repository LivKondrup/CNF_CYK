import CNFConverterArchitecture.AbstractFactory.ConverterForReversingTrees
import CNFConverterArchitecture.ConvertToCNF
import GrammarArchitecture.{Grammar, Lambda, NonTerminal, Rule, Terminal}
import CNFConverterArchitecture.HistoryTreeArchitecture.{HistoryTreeBuilder, HistoryTreeNode}
import ParseTreeArchitecture.{ParseTreeConverter, ParseTreeLeaf, ParseTreeNode}
import javax.swing.text.StyledEditorKit.AlignmentAction
import org.junit.jupiter.api.{BeforeEach, Test}

import scala.collection.mutable.ListBuffer

class ReverseParseTreeTest {
  var grammar: Grammar = null
  @BeforeEach
  def setUp(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B"), NonTerminal("C")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule3 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val rule4 = new Rule(NonTerminal("C"), ListBuffer(Terminal("c")))
    grammar = new Grammar(Set(rule1, rule2, rule3, rule4), NonTerminal("S"))
  }

  @Test
  def correctReversalOfRenaming(): Unit = {
    val factory = new ConverterForReversingTrees()
    val converter = new ConvertToCNF(factory)
    val convertedGrammar = converter.getGrammarOnCNF(grammar)

    val builder = factory.getHistoryTreeBuilder

    val treeAfterCYK = CYKParser.parseAndGetParseTree("abc", convertedGrammar)
    val reverseParseTreeActual = ParseTreeConverter.reverseRenaming(treeAfterCYK, builder)
    val reverseParseTreeExpected = ParseTreeNode(NonTerminal("S"), ListBuffer(
      ParseTreeNode(NonTerminal("A"), ListBuffer(ParseTreeLeaf(Terminal("a")))),
      ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeLeaf(Terminal("b")))),
      ParseTreeNode(NonTerminal("C"), ListBuffer(ParseTreeLeaf(Terminal("c"))))
    ))

    assert(reverseParseTreeActual == reverseParseTreeExpected)
  }

  @Test
  def reverseRenamingOnlyReversesRenaming(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("B"), ListBuffer(NonTerminal("A")))
    val rule3 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))

    grammar = new Grammar(Set(rule1, rule2, rule3), NonTerminal("S"))

    val factory = new ConverterForReversingTrees()
    val converter = new ConvertToCNF(factory)
    val convertedGrammar = converter.getGrammarOnCNF(grammar)
    val treeAfterCYK = CYKParser.parseAndGetParseTree("a", convertedGrammar)

    val historyTreeBuilder = factory.getHistoryTreeBuilder

    val reversedParseTreeActual = ParseTreeConverter.reverseRenaming(treeAfterCYK, historyTreeBuilder)
    val reversedTreeExpected = ParseTreeNode(NonTerminal("S"), ListBuffer(ParseTreeLeaf(Terminal("a"))))

    assert(CYKParser.canParse("a", convertedGrammar))
    assert(reversedParseTreeActual == reversedTreeExpected)
  }

  @Test
  def reverseRenamingWithMultipleRenaming():Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), Terminal("b"), NonTerminal("C"), NonTerminal("D")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule3 = new Rule(NonTerminal("C"), ListBuffer(Terminal("c")))
    val rule4 = new Rule(NonTerminal("D"), ListBuffer(Terminal("d")))

    grammar = new Grammar(Set(rule1, rule2, rule3, rule4), NonTerminal("S"))

    val factory = new ConverterForReversingTrees()
    val converter = new ConvertToCNF(factory)
    val convertedGrammar = converter.getGrammarOnCNF(grammar)

    val treeAfterCYK = CYKParser.parseAndGetParseTree("abcd", convertedGrammar)

    val historyTreeBuilder = factory.getHistoryTreeBuilder
    val reversedParseTreeActual = ParseTreeConverter.reverseRenaming(treeAfterCYK, historyTreeBuilder)
    val reversedTreeExpected =
      ParseTreeNode(NonTerminal("S"), ListBuffer(
        ParseTreeNode(NonTerminal("A"), ListBuffer(ParseTreeLeaf(Terminal("a")))),
        ParseTreeLeaf(Terminal("b")),
        ParseTreeNode(NonTerminal("C"), ListBuffer(ParseTreeLeaf(Terminal("c")))),
        ParseTreeNode(NonTerminal("D"), ListBuffer(ParseTreeLeaf(Terminal("d"))))
      ))
    assert(reversedParseTreeActual == reversedTreeExpected)
  }


  @Test
  def reverseChainsSimpleTest(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))

    grammar = new Grammar(Set(rule1, rule2), NonTerminal("S"))

    val factory = new ConverterForReversingTrees()
    val converter = new ConvertToCNF(factory)
    val convertedGrammar = converter.getGrammarOnCNF(grammar)
    val treeAfterCYK = CYKParser.parseAndGetParseTree("a", convertedGrammar)

    val historyTreeBuilder = factory.getHistoryTreeBuilder
    val chainParses = factory.getChainParses

    val reversedParseTreeActual = ParseTreeConverter.reverseChains(treeAfterCYK, historyTreeBuilder, chainParses)
    val reversedTreeExpected = ParseTreeNode(NonTerminal("S"), ListBuffer(ParseTreeNode(NonTerminal("A"), ListBuffer(ParseTreeLeaf(Terminal("a"))))))

    assert(reversedParseTreeActual == reversedTreeExpected)
  }

  @Test
  def reverseChainsLongerChain(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("B"), ListBuffer(NonTerminal("A")))
    val rule3 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    grammar = new Grammar(Set(rule1, rule2, rule3), NonTerminal("S"))

    val factory = new ConverterForReversingTrees()
    val converter = new ConvertToCNF(factory)
    val convertedGrammar = converter.getGrammarOnCNF(grammar)

    val treeAfterCYK = CYKParser.parseAndGetParseTree("a", convertedGrammar)

    val builder = factory.getHistoryTreeBuilder
    val chainParses = factory.getChainParses

    val reversedParseTreeActual = ParseTreeConverter.reverseChains(treeAfterCYK, builder, chainParses)
    val reversedTreeExpected = ParseTreeNode(NonTerminal("S"), ListBuffer(ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeNode(NonTerminal("A"), ListBuffer(ParseTreeLeaf(Terminal("a"))))))))

    assert(reversedParseTreeActual == reversedTreeExpected)
  }

  @Test
  def reverseLambdaSimpleTest(): Unit ={
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val rule3 = new Rule(NonTerminal("A"), ListBuffer(Lambda()))

    grammar = new Grammar(Set(rule1, rule2, rule3), NonTerminal("S"))

    val factory = new ConverterForReversingTrees()
    val converter = new ConvertToCNF(factory)
    val convertedGrammar = converter.getGrammarOnCNF(grammar)    // to update history tree and lambda parses

    val historyTreeBuilder = factory.getHistoryTreeBuilder
    val lambdaParses = factory.getLambdaParses

    val tree = ParseTreeNode(NonTerminal("S"), ListBuffer(ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeLeaf(Terminal("b"))))))
    val reversedParseTreeActual = ParseTreeConverter.reverseLambda(tree, historyTreeBuilder, lambdaParses)
    val reversedTreeExpected = ParseTreeNode(NonTerminal("S"), ListBuffer(
      ParseTreeNode(NonTerminal("A"), ListBuffer(ParseTreeLeaf(Lambda()))),
      ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeLeaf(Terminal("b"))))
    ))

    assert(reversedParseTreeActual == reversedTreeExpected)
  }

  @Test
  def reverseLambdaAdvancedTest(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(Terminal("a"), NonTerminal("B"), Terminal("c"), NonTerminal("D"), Terminal("e")))
    val rule2 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val rule3 = new Rule(NonTerminal("B"), ListBuffer(Lambda()))
    val rule4 = new Rule(NonTerminal("D"), ListBuffer(Lambda()))

    grammar = new Grammar(Set(rule1, rule2, rule3, rule4), NonTerminal("S"))

    val factory = new ConverterForReversingTrees()
    val converter = new ConvertToCNF(factory)
    val convertedGrammar = converter.getGrammarOnCNF(grammar)    // to update history tree and lambda parses

    val historyTreeBuilder = factory.getHistoryTreeBuilder
    val lambdaParses = factory.getLambdaParses

    val tree = ParseTreeNode(NonTerminal("S"), ListBuffer(
      ParseTreeLeaf(Terminal("a")),
      ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeLeaf(Terminal("b")))),
      ParseTreeNode(NonTerminal("AA"), ListBuffer(
        ParseTreeLeaf(Terminal("c")),
        ParseTreeLeaf(Terminal("e"))
      ))
    ))
    val reversedParseTreeActual = ParseTreeConverter.reverseLambda(tree, historyTreeBuilder, lambdaParses)
    val reversedTreeExpected = ParseTreeNode(NonTerminal("S"), ListBuffer(
      ParseTreeLeaf(Terminal("a")),
      ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeLeaf(Terminal("b")))),
      ParseTreeLeaf(Terminal("c")),
      ParseTreeNode(NonTerminal("D"), ListBuffer(ParseTreeLeaf(Lambda()))),
      ParseTreeLeaf(Terminal("e"))))

    assert(reversedParseTreeActual == reversedTreeExpected)
  }

  @Test
  def reverseAllSteps(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(Terminal("a"), NonTerminal("B"), Terminal("c"), NonTerminal("D"), Terminal("e")))
    val rule2 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val rule3 = new Rule(NonTerminal("B"), ListBuffer(Lambda()))
    val rule5 = new Rule(NonTerminal("B"), ListBuffer(NonTerminal("D")))
    val rule4 = new Rule(NonTerminal("D"), ListBuffer(Lambda()))
    val rule6 = new Rule(NonTerminal("D"), ListBuffer(Terminal("d")))

    grammar = new Grammar(Set(rule1, rule2, rule3, rule4, rule5, rule6), NonTerminal("S"))

    val factory = new ConverterForReversingTrees()
    val converter = new ConvertToCNF(factory)
    val convertedGrammar = converter.getGrammarOnCNF(grammar)
    println(convertedGrammar.getRules())

    assert(CYKParser.canParse("abce", convertedGrammar))
    val treeAfterCYK = CYKParser.parseAndGetParseTree("adce", convertedGrammar)
    val reversedTree = ParseTreeConverter.reverseTreeToOriginalGrammar(treeAfterCYK, factory.getHistoryTreeBuilder, factory.getLambdaParses, factory.getChainParses)

    val expectedTree = ParseTreeNode(NonTerminal("S"), ListBuffer(
      ParseTreeLeaf(Terminal("a")),
      ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeNode(NonTerminal("D"), ListBuffer(ParseTreeLeaf(Terminal("d")))))),
      ParseTreeLeaf(Terminal("c")),
      ParseTreeNode(NonTerminal("D"), ListBuffer(ParseTreeLeaf(Lambda()))),
      ParseTreeLeaf(Terminal("e"))))

    assert(reversedTree == expectedTree)
  }
}
