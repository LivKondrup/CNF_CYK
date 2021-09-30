import CNFConverterArchitecture.AbstractFactory.ConverterForReversingTrees
import CNFConverterArchitecture.ConvertToCNF
import GrammarArchitecture.{Grammar, NonTerminal, Rule, Terminal}
import CNFConverterArchitecture.HistoryTreeArchitecture.NoUpdatingBuilder
import ParseTreeArchitecture.{ParseTreeLeaf, ParseTreeNode}
import org.junit.jupiter.api.Test

import scala.collection.mutable.ListBuffer

class CYKParserTest {

  @Test
  def canNotParseaab(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule3 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val rules1 = Set(rule1, rule2, rule3)
    val grammar = new Grammar(rules1, NonTerminal("S"))

    val parser = CYKParser

    val canParseaab = parser.canParse("aab", grammar)

    assert(!canParseaab)
  }

  @Test
  def canParseab(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule3 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val rules1 = Set(rule1, rule2, rule3)
    val grammar = new Grammar(rules1, NonTerminal("S"))

    val parser = CYKParser

    val canParseab = parser.canParse("ab", grammar)

    assert(canParseab)
  }

  @Test
  def parseForLargerGrammar(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule3 = new Rule(NonTerminal("A"), ListBuffer(NonTerminal("A"), NonTerminal("A")))
    val rule4 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val rule5 = new Rule(NonTerminal("B"), ListBuffer(NonTerminal("B"), NonTerminal("B")))

    val rules1 = Set(rule1, rule2, rule3, rule4, rule5)
    val grammar = new Grammar(rules1, NonTerminal("S"))

    val parser = CYKParser

    val canParseab = parser.canParse("ab", grammar)
    assert(canParseab)

    val canParseaab = parser.canParse("aab", grammar)
    assert(canParseaab)

    val canParseaba = parser.canParse("aba", grammar)
    assert(!canParseaba)
  }

  @Test
  def canConvertParseArrayToTreeab(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule3 = new Rule(NonTerminal("A"), ListBuffer(NonTerminal("A"), NonTerminal("A")))
    val rule4 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val rule5 = new Rule(NonTerminal("B"), ListBuffer(NonTerminal("B"), NonTerminal("B")))

    val rules1 = Set(rule1, rule2, rule3, rule4, rule5)
    val grammar = new Grammar(rules1, NonTerminal("S"))
    val parser = CYKParser

    val tree = parser.parseAndGetParseTree("ab", grammar)
    val expectedTree = ParseTreeNode(grammar.getStartVariable(),
      ListBuffer(ParseTreeNode(NonTerminal("A"),
        ListBuffer(ParseTreeLeaf(Terminal("a")))),
      ParseTreeNode(NonTerminal("B"),
        ListBuffer(ParseTreeLeaf(Terminal("b"))))))

    assert(tree.equals(expectedTree))
  }

  @Test
  def canConvertParseArrayToTreeaabb(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule3 = new Rule(NonTerminal("A"), ListBuffer(NonTerminal("A"), NonTerminal("A")))
    val rule4 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val rule5 = new Rule(NonTerminal("B"), ListBuffer(NonTerminal("B"), NonTerminal("B")))

    val rules1 = Set(rule1, rule2, rule3, rule4, rule5)
    val grammar = new Grammar(rules1, NonTerminal("S"))
    val parser = CYKParser

    val tree = parser.parseAndGetParseTree("aabb", grammar)
    val expectedTree =
      ParseTreeNode(NonTerminal("S"), ListBuffer(
        ParseTreeNode(NonTerminal("A"), ListBuffer(
          ParseTreeNode(NonTerminal("A"), ListBuffer(
            ParseTreeLeaf(Terminal("a"))
          )),
          ParseTreeNode(NonTerminal("A"), ListBuffer(
            ParseTreeLeaf(Terminal("a"))
          ))
        )),
        ParseTreeNode(NonTerminal("B"), ListBuffer(
          ParseTreeNode(NonTerminal("B"), ListBuffer(
            ParseTreeLeaf(Terminal("b"))
          )),
          ParseTreeNode(NonTerminal("B"), ListBuffer(
            ParseTreeLeaf(Terminal("b"))
          ))
        ))
      ))
    assert(tree.equals(expectedTree))
  }

  @Test
  def canParseabc(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B"), NonTerminal("C")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule3 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val rule4 = new Rule(NonTerminal("C"), ListBuffer(Terminal("c")))
    val grammar = new Grammar(Set(rule1, rule2, rule3, rule4), NonTerminal("S"))

    val grammarCNF = new ConvertToCNF(new ConverterForReversingTrees()).getGrammarOnCNF(grammar)


    assert(CYKParser.canParse("abc", grammarCNF))

    val tree = CYKParser.parseAndGetParseTree("abc", grammarCNF)
    val expectedTree = ParseTreeNode(NonTerminal("S"), ListBuffer(
      ParseTreeNode(NonTerminal("AA"), ListBuffer(
        ParseTreeNode(NonTerminal("A"), ListBuffer(ParseTreeLeaf(Terminal("a")))),
        ParseTreeNode(NonTerminal("B"), ListBuffer(ParseTreeLeaf(Terminal("b"))))
      )),
      ParseTreeNode(NonTerminal("C"), ListBuffer(ParseTreeLeaf(Terminal("c"))))
    ))

    assert(tree.equals(expectedTree))
  }

}
