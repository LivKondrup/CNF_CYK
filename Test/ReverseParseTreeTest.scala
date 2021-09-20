import CNFConverterArchitecture.AbstractFactory.ConverterForReversingTrees
import CNFConverterArchitecture.ConvertToCNF
import GrammarArchitecture.{Grammar, Lambda, NonTerminal, Rule, Terminal}
import HistoryTreeArchitecture.{HistoryTreeBuilder, HistoryTreeNode}
import ParseTreeArchitecture.{ParseTreeConverter, ParseTreeLeaf, ParseTreeNode}
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
    val historyTreeBuilder = new HistoryTreeBuilder()
    val convertedGrammar = new ConvertToCNF(new ConverterForReversingTrees()).getGrammarOnCNF(grammar)
    val treeAfterCYK = CYKParser.parseAndGetParseTree("abc", convertedGrammar)
    val reverseParseTreeActual = ParseTreeConverter.reverseRenaming(treeAfterCYK, historyTreeBuilder)
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

    val historyTreeBuilder = new HistoryTreeBuilder()
    val convertedGrammar = new ConvertToCNF(new ConverterForReversingTrees()).getGrammarOnCNF(grammar)
    val treeAfterCYK = CYKParser.parseAndGetParseTree("a", convertedGrammar)
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

    val historyTreeBuilder = new HistoryTreeBuilder()
    val convertedGrammar = new ConvertToCNF(new ConverterForReversingTrees()).getGrammarOnCNF(grammar)
    val treeAfterCYK = CYKParser.parseAndGetParseTree("abcd", convertedGrammar)
    val reversedParseTreeActual = ParseTreeConverter.reverseRenaming(treeAfterCYK, historyTreeBuilder)
    val reversedTreeExpected =
      ParseTreeNode(NonTerminal("S"), ListBuffer(
        ParseTreeNode(NonTerminal("A"), ListBuffer(ParseTreeLeaf(Terminal("a")))),
        ParseTreeLeaf(Terminal("b")),
        ParseTreeNode(NonTerminal("C"), ListBuffer(ParseTreeLeaf(Terminal("c")))),
        ParseTreeNode(NonTerminal("D"), ListBuffer(ParseTreeLeaf(Terminal("d")))),
      ))

    assert(reversedParseTreeActual == reversedTreeExpected)
  }

  @Test
  def reverseChainsSimpleTest(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))

    grammar = new Grammar(Set(rule1, rule2), NonTerminal("S"))

    val historyTreeBuilder = new HistoryTreeBuilder()
    val convertedGrammar = new ConvertToCNF(new ConverterForReversingTrees()).getGrammarOnCNF(grammar)
    val treeAfterCYK = CYKParser.parseAndGetParseTree("a", convertedGrammar)
    val reversedParseTreeActual = ParseTreeConverter.reverseChains(treeAfterCYK, historyTreeBuilder)
    val reversedTreeExpected = ParseTreeNode(NonTerminal("S"), ListBuffer(ParseTreeNode(NonTerminal("A"), ListBuffer(ParseTreeLeaf(Terminal("a"))))))

    assert(reversedParseTreeActual == reversedTreeExpected)
  }

  @Test
  def reverseChainsLongerChain(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("B"), ListBuffer(NonTerminal("A")))
    val rule3 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))

    grammar = new Grammar(Set(rule1, rule2, rule3), NonTerminal("S"))

    val historyTreeBuilder = new HistoryTreeBuilder()
    val convertedGrammar = new ConvertToCNF(new ConverterForReversingTrees()).getGrammarOnCNF(grammar)
    val treeAfterCYK = CYKParser.parseAndGetParseTree("a", convertedGrammar)
    val reversedParseTreeActual = ParseTreeConverter.reverseChains(treeAfterCYK, historyTreeBuilder)
    val reversedTreeExpected = ParseTreeNode(NonTerminal("S"), ListBuffer(ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeNode(NonTerminal("A"), ListBuffer(ParseTreeLeaf(Terminal("a"))))))))

    assert(reversedParseTreeActual == reversedTreeExpected)
  }

  @Test
  def reverseLambdaSimpleTest(): Unit ={
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val rule3 = new Rule(NonTerminal("A"), ListBuffer(Lambda()))

    grammar = new Grammar(Set(rule1, rule2, rule3), NonTerminal("S"))

    val historyTreeBuilder = new HistoryTreeBuilder()
    val converter = new ConvertToCNF(new ConverterForReversingTrees())
    val convertedGrammar = converter.getGrammarOnCNF(grammar)
    val tree = ParseTreeNode(NonTerminal("S"), ListBuffer(ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeLeaf(Terminal("b"))))))
    val reversedParseTreeActual = ParseTreeConverter.reverseLambda(tree, historyTreeBuilder, converter.getLambdaParses)
    val reversedTreeExpected = ParseTreeNode(NonTerminal("S"), ListBuffer(
      ParseTreeNode(NonTerminal("A"), ListBuffer(ParseTreeLeaf(Lambda()))),
      ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeLeaf(Terminal("b"))))
    ))

    println("Actual:   " + reversedParseTreeActual)
    println("Expected: " + reversedTreeExpected)

    assert(reversedParseTreeActual == reversedTreeExpected)
  }
}
