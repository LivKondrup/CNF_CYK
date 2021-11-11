import GrammarArchitecture.{Grammar, NonTerminal, Rule, Terminal}
import GraphArchitecture.{Edge, Graph}
import ParseTreeArchitecture.{ParseTreeLeaf, ParseTreeNode}
import org.junit.jupiter.api.Test

import scala.collection.mutable.ListBuffer

class ReachabilityToTreeTest {

  @Test
  def canGenerateSimpleParseTree(): Unit ={
    val edge1 = new Edge(0,1,Terminal("a"))
    val edge2 = new Edge(1, 2, Terminal("b"))
    val graph = new Graph(Set(edge1, edge2))

    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule3 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val grammar = new Grammar(Set(rule1, rule2, rule3), NonTerminal("S"))

    val actualTree = Reachability.getParseTree(graph, 0, 2, grammar)
    val expectedTree = ParseTreeNode(NonTerminal("S"), ListBuffer(
      ParseTreeNode(NonTerminal("A"), ListBuffer(ParseTreeLeaf(Terminal("a")))),
      ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeLeaf(Terminal("b"))))
    ))

    assert(actualTree == expectedTree)
  }

  @Test
  def cycleGraphTest(): Unit = {
    val edge1 = new Edge(0, 1, Terminal("a"))
    val edge2 = new Edge(1, 0, Terminal("b"))
    val graph = new Graph(Set(edge1, edge2))

    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule3 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val grammar = new Grammar(Set(rule1, rule2, rule3), NonTerminal("S"))

    val actualTree = Reachability.getParseTree(graph, 0, 0, grammar)
    val expectedTree = ParseTreeNode(NonTerminal("S"), ListBuffer(
      ParseTreeNode(NonTerminal("A"), ListBuffer(ParseTreeLeaf(Terminal("a")))),
      ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeLeaf(Terminal("b"))))
    ))

    assert(actualTree == expectedTree)
  }

  @Test
  def multipleLayersOfGrammarTest(): Unit = {
    val edge1 = new Edge(0, 1, Terminal("a"))
    val edge2 = new Edge(1, 2, Terminal("b"))
    val edge3 = new Edge(2, 3, Terminal("c"))
    val graph = new Graph(Set(edge1, edge2, edge3))

    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("K"), NonTerminal("C")))
    val rule2 = new Rule(NonTerminal("K"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule3 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule4 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val rule5 = new Rule(NonTerminal("C"), ListBuffer(Terminal("c")))
    val grammar = new Grammar(Set(rule1, rule2, rule3, rule4, rule5), NonTerminal("S"))

    val actualTree = Reachability.getParseTree(graph, 0, 3, grammar)
    val expectedTree = ParseTreeNode(NonTerminal("S"), ListBuffer(
      ParseTreeNode(NonTerminal("K"), ListBuffer(
        ParseTreeNode(NonTerminal("A"), ListBuffer(ParseTreeLeaf(Terminal("a")))),
        ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeLeaf(Terminal("b"))))
      )),
      ParseTreeNode(NonTerminal("C"), ListBuffer(ParseTreeLeaf(Terminal("c"))))
    ))

    assert(actualTree == expectedTree)
  }

  @Test
  def cycleGraphAndDeeperGrammar(): Unit ={
    val edge1 = new Edge(0, 1, Terminal("a"))
    val edge2 = new Edge(1,2, Terminal("b"))
    val edge3 = new Edge(2,0, Terminal("c"))
    val edge4 = new Edge(2,3, Terminal("d"))
    val graph = new Graph(Set(edge1, edge2, edge3, edge4))

    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("K"), NonTerminal("L")))
    val rule2 = new Rule(NonTerminal("K"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule3 = new Rule(NonTerminal("L"), ListBuffer(NonTerminal("M"), NonTerminal("N")))
    val rule4 = new Rule(NonTerminal("M"), ListBuffer(NonTerminal("C"), NonTerminal("A")))
    val rule5 = new Rule(NonTerminal("N"), ListBuffer(NonTerminal("B"), NonTerminal("D")))
    val rule6 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule7 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val rule8 = new Rule(NonTerminal("C"), ListBuffer(Terminal("c")))
    val rule9 = new Rule(NonTerminal("D"), ListBuffer(Terminal("d")))
    val grammar = new Grammar(Set(rule1, rule2, rule3, rule4, rule5, rule6, rule7, rule8, rule9), NonTerminal("S"))

    val actualGrammar = Reachability.getParseTree(graph, 0, 3, grammar)
    val expectedGrammar = ParseTreeNode(NonTerminal("S"), ListBuffer(
      ParseTreeNode(NonTerminal("K"), ListBuffer(
        ParseTreeNode(NonTerminal("A"), ListBuffer(ParseTreeLeaf(Terminal("a")))),
        ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeLeaf(Terminal("b"))))
      )),
      ParseTreeNode(NonTerminal("L"), ListBuffer(
        ParseTreeNode(NonTerminal("M"), ListBuffer(
          ParseTreeNode(NonTerminal("C"), ListBuffer(ParseTreeLeaf(Terminal("c")))),
          ParseTreeNode(NonTerminal("A"), ListBuffer(ParseTreeLeaf(Terminal("a"))))
        )),
        ParseTreeNode(NonTerminal("N"), ListBuffer(
          ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeLeaf(Terminal("b")))),
          ParseTreeNode(NonTerminal("D"), ListBuffer(ParseTreeLeaf(Terminal("d"))))
        ))
      ))
    ))

    assert(actualGrammar == expectedGrammar)
  }
}
