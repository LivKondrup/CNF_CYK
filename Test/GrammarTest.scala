import GrammarArchitecture.{Grammar, NonTerminal, Rule, Terminal}
import org.junit.jupiter.api.{BeforeEach, Test}

import scala.collection.mutable.ListBuffer

class GrammarTest {
  @Test
  def equalWorksCorrect(): Unit ={
    val rule1 = new Rule(new NonTerminal("S"), ListBuffer(new Terminal("a"), new NonTerminal("A")))
    val rule2 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("a"), new NonTerminal("B")))
    val grammar1:Grammar = new Grammar(Set(rule1, rule2), new NonTerminal("S"))

    val rule3 = new Rule(new NonTerminal("S"), ListBuffer(new Terminal("a"), new NonTerminal("A")))
    val rule4 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("a"), new NonTerminal("B")))
    val grammar2:Grammar = new Grammar(Set(rule4, rule3), new NonTerminal("S"))

    assert(grammar1.equals(grammar2))
  }
}
