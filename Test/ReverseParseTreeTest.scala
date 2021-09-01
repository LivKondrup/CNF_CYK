import GrammarArchitecture.{Grammar, NonTerminal, Rule, Terminal}
import HistoryTreeArchitecture.{HistoryTreeBuilder, HistoryTreeNode}
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
    val historyTreeBuilder = new HistoryTreeBuilder(grammar)
    val convertedGrammar = new ConvertToCNF(historyTreeBuilder).getGrammarOnCNF(grammar)
    val treeAfterCYK = CYKParser.parseAndGetParseTree("abc", convertedGrammar)
    val reverseParseTreeActual = ParseTreeConverter.reverseRenaming(treeAfterCYK, historyTreeBuilder)
    val reverseParseTreeExpected = ParseTreeNode(NonTerminal("S"), ListBuffer(
      ParseTreeNode(NonTerminal("A"), ListBuffer(ParseTreeLeaf(Terminal("a")))),
      ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeLeaf(Terminal("b")))),
      ParseTreeNode(NonTerminal("C"), ListBuffer(ParseTreeLeaf(Terminal("c"))))
    ))

    println("Expected" + reverseParseTreeExpected)
    println("Actual: " + reverseParseTreeActual)
    println("After CYK: " + treeAfterCYK)


    assert(reverseParseTreeActual == reverseParseTreeExpected)
  }
}
