import org.junit.jupiter.api.{BeforeAll, BeforeEach, Test}

import scala.collection.mutable.ListBuffer

class BuildHistoryTreeTest {
  var builder: HistoryTreeBuilder = null

  @BeforeEach
  def setUp(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a"), NonTerminal("A")))
    val rule3 = new Rule(NonTerminal("B"), ListBuffer(NonTerminal("B"), Terminal("b")))
    val grammar = new Grammar(Set(rule1, rule2, rule3), NonTerminal("S"))
    builder = new HistoryTreeBuilder(grammar)
  }

  @Test
  def isSetUpCorrect(): Unit = {
    val trees = builder.getHistoryTrees()
    val expectedTree1 = new HistoryTreeNode(new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B"))), ListBuffer(Leaf), 0)
    assert(trees.contains(expectedTree1))

    val expectedTree2 = new HistoryTreeNode(new Rule(NonTerminal("A"), ListBuffer(Terminal("a"), NonTerminal("A"))), ListBuffer(Leaf), 0)
    assert(trees.contains(expectedTree2))

    val expectedTree3 = new HistoryTreeNode(new Rule(NonTerminal("B"), ListBuffer(NonTerminal("B"), Terminal("b"))), ListBuffer(Leaf), 0)
    assert(trees.contains(expectedTree3))
  }

  @Test
  def UpdatesCorrectly(): Unit = {
    val oldRule = new Rule(NonTerminal("A"), ListBuffer(Terminal("a"), NonTerminal("A")))
    val newRule = new Rule(NonTerminal("A"), ListBuffer(NonTerminal("C"), NonTerminal("A")))
    builder.ruleUpdated(oldRule, newRule, 1)
    val trees = builder.getHistoryTrees()
    val expectedNewChildren:ListBuffer[HistoryTree] = ListBuffer(Leaf, new HistoryTreeNode(newRule, ListBuffer(Leaf), 1))
    val expectedNewTree = new HistoryTreeNode(new Rule(NonTerminal("A"), ListBuffer(Terminal("a"), NonTerminal("A"))), expectedNewChildren, 0)
    assert (trees.contains(expectedNewTree))
  }
}
