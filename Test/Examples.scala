import org.junit.jupiter.api.{BeforeEach, Test}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


class Examples {

  @Test
  def example1():Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(Lambda()))
    val rule2 = new Rule(NonTerminal("S"), ListBuffer(Terminal("a"), NonTerminal("B")))
    val rule3 = new Rule(NonTerminal("S"), ListBuffer(Terminal("b"), NonTerminal("A")))
    val rule4 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a"), NonTerminal("S")))
    val rule5 = new Rule(NonTerminal("A"), ListBuffer(Terminal("b"), NonTerminal("A"), NonTerminal("A")))
    val rule6 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b"), NonTerminal("S")))
    val rule7 = new Rule(NonTerminal("B"), ListBuffer(Terminal("a"), NonTerminal("B"), NonTerminal("B")))
    val grammarOriginal = new Grammar(Set(rule1, rule2, rule3, rule4, rule5, rule6, rule7), NonTerminal("S"))
    val grammarConverted = ConvertToCNF.getGrammarOnCNF(grammarOriginal)
    println(grammarConverted.getRules().size)
    println(grammarConverted.getRules())
    assert(grammarConverted.getRules().size >= 12)
  }
}
