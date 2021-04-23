import org.junit.jupiter.api.{BeforeEach, Test}

import scala.collection.mutable.ListBuffer

class RuleTest {
  @Test
  def equalsWorksCorrect(): Unit ={
    val rule1 = new Rule (new NonTerminal("S"), ListBuffer(new NonTerminal("A"), new NonTerminal("B")))
    val rule2 = new Rule (new NonTerminal("S"), ListBuffer(new NonTerminal("A"), new NonTerminal("B")))

    assert(rule1 == rule2)
  }

}
