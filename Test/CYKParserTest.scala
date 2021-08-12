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

    val parser = new CYKParser()

    val canParseaab = parser.parse("aab", grammar)

    assert(!canParseaab)
  }

  @Test
  def canParseab(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule3 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val rules1 = Set(rule1, rule2, rule3)
    val grammar = new Grammar(rules1, NonTerminal("S"))

    val parser = new CYKParser()

    val canParseab = parser.parse("ab", grammar)

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

    val parser = new CYKParser()

    val canParseab = parser.parse("ab", grammar)
    assert(canParseab)

    val canParseaab = parser.parse("aab", grammar)
    assert(canParseaab)

    val canParseaba = parser.parse("aba", grammar)
    assert(!canParseaba)
  }

}
