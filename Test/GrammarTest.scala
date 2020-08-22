import org.junit.jupiter.api.{BeforeEach, Test}

class GrammarTest {
  @Test
  def equalWorksCorrect(): Unit ={
    val rule1 = new Rule("S", Set("a", "A"))
    val rule2 = new Rule("A", Set("a", "B"))
    val grammar1:Grammar = new Grammar(Set(rule1, rule2), "S")

    val rule3 = new Rule("S", Set("a", "A"))
    val rule4 = new Rule("A", Set("a", "B"))
    val grammar2:Grammar = new Grammar(Set(rule4, rule3), "S")

    assert(grammar1.equals(grammar2))
  }
}
