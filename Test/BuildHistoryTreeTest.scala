import CNFConverterArchitecture.AbstractFactory.ConverterForReversingTrees
import CNFConverterArchitecture.ConvertToCNF
import GrammarArchitecture.{Grammar, Lambda, NonTerminal, Rule, Terminal}
import HistoryTreeArchitecture.{HistoryTree, HistoryTreeBuilder, HistoryTreeLeaf, HistoryTreeNode}
import org.junit.jupiter.api.{BeforeAll, BeforeEach, Test}

import scala.collection.mutable.ListBuffer

class BuildHistoryTreeTest {
  var builder: HistoryTreeBuilder = _
  var grammar:Grammar = _

  @BeforeEach
  def setUp(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a"), NonTerminal("A")))
    val rule3 = new Rule(NonTerminal("B"), ListBuffer(NonTerminal("B"), Terminal("b")))
    grammar = new Grammar(Set(rule1, rule2, rule3), NonTerminal("S"))
    builder = new HistoryTreeBuilder()
  }

  @Test
  def isSetUpCorrect(): Unit = {
    builder.init(grammar)
    val trees = builder.getHistoryTrees()
    val expectedTree1 = new HistoryTreeNode(new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B"))), Set(HistoryTreeLeaf), 0)
    assert(trees.contains(expectedTree1))

    val expectedTree2 = new HistoryTreeNode(new Rule(NonTerminal("A"), ListBuffer(Terminal("a"), NonTerminal("A"))), Set(HistoryTreeLeaf), 0)
    assert(trees.contains(expectedTree2))

    val expectedTree3 = new HistoryTreeNode(new Rule(NonTerminal("B"), ListBuffer(NonTerminal("B"), Terminal("b"))), Set(HistoryTreeLeaf), 0)
    assert(trees.contains(expectedTree3))
  }

  @Test
  def UpdatesCorrectly(): Unit = {
    val oldRule = new Rule(NonTerminal("A"), ListBuffer(Terminal("a"), NonTerminal("A")))
    val newRule = new Rule(NonTerminal("A"), ListBuffer(NonTerminal("C"), NonTerminal("A")))
    builder = new HistoryTreeBuilder()
    builder.init(grammar)
    builder.ruleUpdated(oldRule, newRule, 1)
    val trees = builder.getHistoryTrees()
    val expectedNewChildren:Set[HistoryTree] = Set(HistoryTreeLeaf, HistoryTreeNode(newRule, Set(HistoryTreeLeaf), 1))
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
    val converter = new ConvertToCNF(new ConverterForReversingTrees())

    builder = converter.getRuleUpdatingBuilder().asInstanceOf[HistoryTreeBuilder]
    builder.init(grammar)
    converter.eliminateLambda(grammar)

    val trees = builder.getHistoryTrees()
    val newRuleNode = HistoryTreeNode(new Rule(NonTerminal("S"), ListBuffer(NonTerminal("B"))), Set(HistoryTreeLeaf), 1)
    val originalRuleNode = HistoryTreeNode(rule1, Set(HistoryTreeLeaf), 1)
    val expectedTreeFromSRule = HistoryTreeNode(rule1, Set(HistoryTreeLeaf, originalRuleNode, newRuleNode), 0)

    assert(trees.contains(expectedTreeFromSRule))
  }

  @Test
  def eliminatesChainsCorrectly(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(NonTerminal("B")))
    val rule3 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val grammar = new Grammar(Set(rule1, rule2, rule3), NonTerminal("S"))
    val converter = new ConvertToCNF(new ConverterForReversingTrees())

    builder.init(grammar)
    builder = converter.getRuleUpdatingBuilder().asInstanceOf[HistoryTreeBuilder]
    converter.eliminateChains(grammar)

    val treeSB = builder.findTreeWithRule(new Rule(NonTerminal("S"), ListBuffer(Terminal("b"))))
    assert(builder.treeContainsRule(treeSB, new Rule(NonTerminal("S"), ListBuffer(NonTerminal("B")))))
  }

  @Test
  def renamesCorrectly(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(Terminal("a"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val grammar = new Grammar(Set(rule1, rule2), NonTerminal("S"))
    val converter = new ConvertToCNF(new ConverterForReversingTrees())

    builder = converter.getRuleUpdatingBuilder().asInstanceOf[HistoryTreeBuilder]
    builder.init(grammar)
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

    val converter = new ConvertToCNF(new ConverterForReversingTrees())

    builder = converter.getRuleUpdatingBuilder().asInstanceOf[HistoryTreeBuilder]
    builder.init(grammar)
    converter.getGrammarOnCNF(grammar)

    // S rule tree has S->B
    val treeS = builder.findTreeWithRule(rule1)
    val expectedSBRule = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("B")))
    assert(builder.treeContainsRule(treeS, expectedSBRule))

    // the tree with the rule S->b also has S->B
    val treeSB = builder.findTreeWithRule(new Rule(NonTerminal("S"), ListBuffer(Terminal("b"))))
    assert(builder.treeContainsRule(treeSB, new Rule(NonTerminal("S"), ListBuffer(NonTerminal("B")))))
  }
}
