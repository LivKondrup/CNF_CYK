import GrammarArchitecture.{Grammar, Lambda, NonTerminal, Rule, Terminal}
import HistoryTreeArchitecture.{HistoryTree, HistoryTreeBuilder, HistoryTreeNode, Leaf}
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
    val expectedTree1 = new HistoryTreeNode(new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B"))), Set(Leaf), 0)
    assert(trees.contains(expectedTree1))

    val expectedTree2 = new HistoryTreeNode(new Rule(NonTerminal("A"), ListBuffer(Terminal("a"), NonTerminal("A"))), Set(Leaf), 0)
    assert(trees.contains(expectedTree2))

    val expectedTree3 = new HistoryTreeNode(new Rule(NonTerminal("B"), ListBuffer(NonTerminal("B"), Terminal("b"))), Set(Leaf), 0)
    assert(trees.contains(expectedTree3))
  }

  @Test
  def UpdatesCorrectly(): Unit = {
    val oldRule = new Rule(NonTerminal("A"), ListBuffer(Terminal("a"), NonTerminal("A")))
    val newRule = new Rule(NonTerminal("A"), ListBuffer(NonTerminal("C"), NonTerminal("A")))
    builder.ruleUpdated(oldRule, newRule, 1)
    val trees = builder.getHistoryTrees()
    val expectedNewChildren:Set[HistoryTree] = Set(Leaf, HistoryTreeNode(newRule, Set(Leaf), 1))
    val expectedNewTree = new HistoryTreeNode(new Rule(NonTerminal("A"), ListBuffer(Terminal("a"), NonTerminal("A"))), expectedNewChildren, 0)
    assert (trees.contains(expectedNewTree))
  }

  @Test
  def removesLamdasCorrectly(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule3 = new Rule(NonTerminal("A"), ListBuffer(Lambda()))
    val rule4 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val grammar = new Grammar(Set(rule1, rule2, rule3, rule4), NonTerminal("S"))
    builder = new HistoryTreeBuilder(grammar)
    val converter = new ConvertToCNF(builder)

    converter.eliminateLambda(grammar)

    val trees = builder.getHistoryTrees()
    val newRuleNode = HistoryTreeNode(new Rule(NonTerminal("S"), ListBuffer(NonTerminal("B"))), Set(Leaf), 1)
    val originalRuleNode = HistoryTreeNode(rule1, Set(Leaf), 1)
    val expectedTreeFromSRule = HistoryTreeNode(rule1, Set(Leaf, originalRuleNode, newRuleNode), 0)

    assert(trees.contains(expectedTreeFromSRule))
  }

  @Test
  def eliminatesChainsCorrectly(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(NonTerminal("B")))
    val rule3 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val grammar = new Grammar(Set(rule1, rule2, rule3), NonTerminal("S"))
    builder = new HistoryTreeBuilder(grammar)
    val converter = new ConvertToCNF(builder)

    converter.eliminateChains(grammar)

    val trees = builder.getHistoryTrees()

    // S rule has S->b, A rule has A->b and B rule has B->b
    val treeS = builder.findTreeWithRule(rule1)
    val expectedSRule = new Rule(NonTerminal("S"), ListBuffer(Terminal("b")))
    assert(builder.treeContainsRule(treeS, expectedSRule))

    val treeA = builder.findTreeWithRule(rule2)
    val expectedARule = new Rule(NonTerminal("A"), ListBuffer(Terminal("b")))
    assert(builder.treeContainsRule(treeA, expectedARule))

    val treeB = builder.findTreeWithRule(rule3)
    val expectedBRule = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    assert(builder.treeContainsRule(treeB, expectedBRule))
  }

  @Test
  def renamesCorrectly(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(Terminal("a"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val grammar = new Grammar(Set(rule1, rule2), NonTerminal("S"))
    builder = new HistoryTreeBuilder(grammar)
    val converter = new ConvertToCNF(builder)

    converter.fixRightSides(grammar)

    val trees = builder.getHistoryTrees()

    // S rule tree has S->AB
    val treeS = builder.findTreeWithRule(rule1)
    val expectedSRule = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("AA"), NonTerminal("B")))   // AA is the first available new name for a variable
    assert(builder.treeContainsRule(treeS, expectedSRule))
  }

  @Test
  def wholeConversionCorrect(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Lambda()))
    val rule3 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule4 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val grammar = new Grammar(Set(rule1, rule2, rule3, rule4), NonTerminal("S"))
    builder = new HistoryTreeBuilder(grammar)
    val converter = new ConvertToCNF(builder)

    converter.getGrammarOnCNF(grammar)

    val trees = builder.getHistoryTrees()

    // S rule tree has S->B and S->b
    val treeS = builder.findTreeWithRule(rule1)
    println(treeS)
    val expectedSBRule = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("B")))
    val expectedSbRule = new Rule(NonTerminal("S"), ListBuffer(Terminal("b")))
    assert(builder.treeContainsRule(treeS, expectedSBRule))
    assert(builder.treeContainsRule(treeS, expectedSbRule))
  }
}
