import GrammarArchitecture.{Grammar, NonTerminal, Rule, Terminal}
import HistoryTreeArchitecture.{HistoryTreeBuilder, HistoryTreeNode}
import org.junit.jupiter.api.{BeforeEach, Test}


import scala.collection.mutable.ListBuffer

class ReverseDerivationTreeTest {
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
    val treeAfterCYK = CYKParser.parseAndGetDerivationTree("abc", convertedGrammar)
    val reverseDerivationTreeActual = DerivationTreeConverter.reverseRenaming(treeAfterCYK, historyTreeBuilder)
    val reverseDerivationTreeExpected = DerivationTreeNode(NonTerminal("S"), ListBuffer(
      DerivationTreeNode(NonTerminal("A"), ListBuffer(Leaf(Terminal("a")))),
      DerivationTreeNode(NonTerminal("B"), ListBuffer(Leaf(Terminal("b")))),
      DerivationTreeNode(NonTerminal("C"), ListBuffer(Leaf(Terminal("c"))))
    ))

    println("Expected" + reverseDerivationTreeExpected)
    println("Actual: " + reverseDerivationTreeActual)
    println("After CYK: " + treeAfterCYK)


    assert(reverseDerivationTreeActual == reverseDerivationTreeExpected)
  }
}
