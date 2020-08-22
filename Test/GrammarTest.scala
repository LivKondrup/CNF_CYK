import org.junit.jupiter.api.{BeforeEach, Test}

class GrammarTest {
  @Test
  def equalWorksCorrect(): Unit ={
    val rule1 = new Rule("S", List("a", "A"))
    val rule2 = new Rule("A", List("a", "B"))
    val grammar1:Grammar = new Grammar(Set(rule1, rule2), "S")

    val rule3 = new Rule("S", List("a", "A"))
    val rule4 = new Rule("A", List("a", "B"))
    val grammar2:Grammar = new Grammar(Set(rule4, rule3), "S")

    assert(grammar1.equals(grammar2))
  }

  @Test
  def equalWorksCorrect2(): Unit ={
    val rule6:Rule = new Rule("S", List("A"))
    val rule7:Rule = new Rule("S", List("B"))
    val rule8:Rule = new Rule("S", List("A", "B"))
    val rule9:Rule = new Rule("A", List("B"))
    val rule10:Rule = new Rule("A", List("a"))
    val rule11:Rule = new Rule("A", List("a" ,"A"))
    val rule12:Rule = new Rule("B", List("b"))
    val rules1: Set[Rule] = Set(rule6, rule7, rule8, rule9, rule10, rule11, rule12)
    val grammar1: Grammar = new Grammar(rules1, "S")

    val rule13:Rule = new Rule("S", List("A"))
    val rule14:Rule = new Rule("S", List("B"))
    val rule15:Rule = new Rule("S", List("A", "B"))
    val rule16:Rule = new Rule("A", List("B"))
    val rule17:Rule = new Rule("A", List("a"))
    val rule18:Rule = new Rule("A", List("a" ,"A"))
    val rule19:Rule = new Rule("B", List("b"))

    val rules2: Set[Rule] = Set(rule15, rule19, rule13, rule14, rule17, rule16, rule18)
    val grammar2: Grammar = new Grammar(rules2, "S")

    val rule20:Rule = new Rule("S", List("A"))
    val rules3: Set[Rule] = Set(rule20)
    val grammar3 = new Grammar(rules3, "S")

    grammar1.equals(grammar3)

    grammar1.equals(grammar2)
    assert (grammar1.equals(grammar2))
  }

}
