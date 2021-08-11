import org.junit.jupiter.api.{BeforeEach, Test}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// Most of these tests are of grammars from exercises in John Martin, Introduction to Languages and the Theory of Computation

class MoreTests {
  var converter = new ConvertToCNF(new NoUpdatingBuilder())
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
    val grammarConverted = converter.getGrammarOnCNF(grammarOriginal)
    assert(grammarConverted.getRules().size >= 12)
  }

  @Test
  def cyclingChain():Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(NonTerminal("B")))
    val rule3 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule4 = new Rule(NonTerminal("B"), ListBuffer(NonTerminal("S")))
    val rule5 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))

    val grammarOriginal = new Grammar(Set(rule1, rule2, rule3, rule4, rule5), NonTerminal("S"))
    val grammarConverted = converter.getGrammarOnCNF(grammarOriginal)

    val rule6 = new Rule(NonTerminal("S"), ListBuffer(Terminal("a")))
    val rule7 = new Rule(NonTerminal("S"), ListBuffer(Terminal("b")))
    val rule8 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule9 = new Rule(NonTerminal("A"), ListBuffer(Terminal("b")))
    val rule10 = new Rule(NonTerminal("B"), ListBuffer(Terminal("a")))
    val rule11 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))

    val grammarExpected = new Grammar(Set(rule6, rule7, rule8, rule9, rule10, rule11), NonTerminal("S"))

    assert(grammarConverted.equals(grammarExpected))

  }

  @Test
  def example2():Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(Lambda()))
    val rule2 = new Rule(NonTerminal("S"), ListBuffer(Terminal("a"), NonTerminal("S")))
    val rule3 = new Rule(NonTerminal("S"), ListBuffer(Terminal("b"), NonTerminal("S")))

    val grammarOriginal = new Grammar(Set(rule1, rule2, rule3), NonTerminal("S"))
    val grammarConverted = converter.getGrammarOnCNF(grammarOriginal)
    assert(grammarConverted.getRules().size == 6)
  }

  @Test
  def example3():Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("S"), NonTerminal("S")))
    val rule2 = new Rule(NonTerminal("S"), ListBuffer(Terminal("b"), NonTerminal("S")))
    val rule3 = new Rule(NonTerminal("S"), ListBuffer(Terminal("a")))

    val grammarOriginal = new Grammar(Set(rule1, rule2, rule3), NonTerminal("S"))
    val grammarConverted = converter.getGrammarOnCNF(grammarOriginal)
    assert(grammarConverted.getRules().size == 4)
  }

}
