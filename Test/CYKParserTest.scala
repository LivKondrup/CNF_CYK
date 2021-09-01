import GrammarArchitecture.{Grammar, NonTerminal, Rule, Terminal}
import HistoryTreeArchitecture.NoUpdatingBuilder
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

    val tree = parser.parseAndGetDerivationTree("ab", grammar)
    val expectedTree = DerivationTreeNode(grammar.getStartVariable(),
      ListBuffer(DerivationTreeNode(NonTerminal("A"),
        ListBuffer(Leaf(Terminal("a")))),
      DerivationTreeNode(NonTerminal("B"),
        ListBuffer(Leaf(Terminal("b"))))))

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

    val tree = parser.parseAndGetDerivationTree("aabb", grammar)
    val expectedTree =
      DerivationTreeNode(NonTerminal("S"), ListBuffer(
        DerivationTreeNode(NonTerminal("A"), ListBuffer(
          DerivationTreeNode(NonTerminal("A"), ListBuffer(
            Leaf(Terminal("a"))
          )),
          DerivationTreeNode(NonTerminal("A"), ListBuffer(
            Leaf(Terminal("a"))
          ))
        )),
        DerivationTreeNode(NonTerminal("B"), ListBuffer(
          DerivationTreeNode(NonTerminal("B"), ListBuffer(
            Leaf(Terminal("b"))
          )),
          DerivationTreeNode(NonTerminal("B"), ListBuffer(
            Leaf(Terminal("b"))
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

    val grammarCNF = new ConvertToCNF(new NoUpdatingBuilder).getGrammarOnCNF(grammar)


    assert(CYKParser.canParse("abc", grammarCNF))

    val tree = CYKParser.parseAndGetDerivationTree("abc", grammarCNF)
    println(tree)
    val expectedTree = DerivationTreeNode(NonTerminal("S"), ListBuffer(
      DerivationTreeNode(NonTerminal("AA"), ListBuffer(
        DerivationTreeNode(NonTerminal("A"), ListBuffer(Leaf(Terminal("a")))),
        DerivationTreeNode(NonTerminal("B"), ListBuffer(Leaf(Terminal("b"))))
      )),
      DerivationTreeNode(NonTerminal("C"), ListBuffer(Leaf(Terminal("c"))))
    ))
    println(expectedTree)

    assert(tree.equals(expectedTree))
  }

}
