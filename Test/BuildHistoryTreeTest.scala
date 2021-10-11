import CNFConverterArchitecture.AbstractFactory.{ConverterForReversingTrees, SimpleConverter}
import CNFConverterArchitecture.ConvertToCNF
import GrammarArchitecture.{Grammar, Lambda, NonTerminal, Rule, Terminal}
import CNFConverterArchitecture.HistoryTreeArchitecture.{HistoryTree, HistoryTreeBuilder, HistoryTreeLeaf, HistoryTreeNode}
import org.junit.jupiter.api.{BeforeAll, BeforeEach, Test}

import scala.collection.mutable.ListBuffer

class BuildHistoryTreeTest {


  @Test
  def isSetUpCorrect(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a"), NonTerminal("A")))
    val rule3 = new Rule(NonTerminal("B"), ListBuffer(NonTerminal("B"), Terminal("b")))
    val grammar = new Grammar(Set(rule1, rule2, rule3), NonTerminal("S"))

    val builder = new HistoryTreeBuilder()
    builder.init(grammar, 1)
    val trees = builder.getHistoryTrees(1)
    val expectedTree1 = new HistoryTreeNode(new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B"))), Set(HistoryTreeLeaf), 0)
    assert(trees.contains(expectedTree1))

    val expectedTree2 = new HistoryTreeNode(new Rule(NonTerminal("A"), ListBuffer(Terminal("a"), NonTerminal("A"))), Set(HistoryTreeLeaf), 0)
    assert(trees.contains(expectedTree2))

    val expectedTree3 = new HistoryTreeNode(new Rule(NonTerminal("B"), ListBuffer(NonTerminal("B"), Terminal("b"))), Set(HistoryTreeLeaf), 0)
    assert(trees.contains(expectedTree3))
  }

  @Test
  def UpdatesCorrectly(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a"), NonTerminal("A")))
    val rule3 = new Rule(NonTerminal("B"), ListBuffer(NonTerminal("B"), Terminal("b")))
    val grammar = new Grammar(Set(rule1, rule2, rule3), NonTerminal("S"))

    val oldRule = new Rule(NonTerminal("A"), ListBuffer(Terminal("a"), NonTerminal("A")))
    val newRule = new Rule(NonTerminal("A"), ListBuffer(NonTerminal("C"), NonTerminal("A")))

    val builder = new HistoryTreeBuilder()

    builder.init(grammar, 1)
    builder.ruleUpdated(oldRule, newRule, 1)
    val trees = builder.getHistoryTrees(1)
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

    val factory = new ConverterForReversingTrees()
    val converter = new ConvertToCNF(factory)

    val builder = factory.getHistoryTreeBuilder
    builder.init(grammar, 1)
    converter.eliminateLambda(grammar)

    // It is expected that the tree with rule S->AB in the root has the rule S->B
    val trees = builder.getHistoryTrees(1)
    val treeS_AB = builder.findTreeWithRule(rule1, 1)
    val ruleSB = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("B")))

    assert(builder.treeContainsRule(treeS_AB, ruleSB))
  }

  @Test
  def eliminatesChainsCorrectly(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(NonTerminal("B")))
    val rule3 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val grammar = new Grammar(Set(rule1, rule2, rule3), NonTerminal("S"))

    val factory = new ConverterForReversingTrees()
    val converter = new ConvertToCNF(factory)

    val builder = factory.getHistoryTreeBuilder
    builder.init(grammar, 2)
    converter.eliminateChains(grammar)

    val treeSB = builder.findTreeWithRule(new Rule(NonTerminal("S"), ListBuffer(Terminal("b"))), 2)
    assert(builder.treeContainsRule(treeSB, new Rule(NonTerminal("S"), ListBuffer(Lambda(), NonTerminal("B")))))
  }

  // tests that the builder has the rule that is expected to replace the rule that is not on CNF
  @Test
  def renamesCorrectly(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(Terminal("a"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val grammar = new Grammar(Set(rule1, rule2), NonTerminal("S"))

    val factory = new ConverterForReversingTrees()
    val converter = new ConvertToCNF(factory)

    val builder = factory.getHistoryTreeBuilder
    builder.init(grammar, 3)
    converter.fixRightSides(grammar)

    // S rule tree has S->AB
    val treeS = builder.findTreeWithRule(rule1, 3)
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

    val factory = new ConverterForReversingTrees()
    val converter = new ConvertToCNF(factory)

    val builder = factory.getHistoryTreeBuilder

    converter.getGrammarOnCNF(grammar)

    // the tree with the rule S->b also has S->B
    val treeSB = builder.findTreeWithRule(new Rule(NonTerminal("S"), ListBuffer(Terminal("b"))), 2)
    assert(builder.treeContainsRule(treeSB, new Rule(NonTerminal("S"), ListBuffer(Lambda(), NonTerminal("B")))))
  }
}
