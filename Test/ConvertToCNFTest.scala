import org.junit.jupiter.api.{BeforeEach, Test}

class ConvertToCNFTest {
  var grammar1:Grammar = _   // A grammar for the tests to use
  @BeforeEach
  def setUp(): Unit ={
    val rule1:Rule = new Rule("S", Set("A", "B"))    // Creating rules for the grammar that should be converted
    val rule2:Rule = new Rule("A", Set("B"))
    val rule3:Rule = new Rule("A", Set("a", "A"))
    val rule4:Rule = new Rule("B", Set("b"))
    val rule5:Rule = new Rule("B", Set("lambda"))
    val rules: Set[Rule] = Set(rule1, rule2, rule3, rule4, rule5) //List of rules in the grammar
    grammar1 = new Grammar(rules, "S") // The grammar
  }

  @Test def grammarCorrectAfterRemoveLambdaMethod(): Unit = {
    val rule6:Rule = new Rule("S", Set("A"))   // Rules of the grammar that grammar1 should be converted to
    val rule7:Rule = new Rule("S", Set("B"))
    val rule8:Rule = new Rule("S", Set("A", "B"))
    val rule9:Rule = new Rule("A", Set("B"))
    val rule10:Rule = new Rule("A", Set("a"))
    val rule11:Rule = new Rule("A", Set("a" ,"A"))
    val rule12:Rule = new Rule("B", Set("b"))
    val rules2: Set[Rule] = Set(rule6, rule7, rule8, rule9, rule10, rule11, rule12)
    val grammar2: Grammar = new Grammar(rules2, "S") // How the grammar should look after being converted

    val grammar3 = ConvertToCNF.eliminateLambda(grammar1)    //Converting grammar to not have lambda rules
    assert(grammar3.equals(grammar2))
  }

  @Test
  def findAllNullableVaribles(): Unit ={
    val expectedNullables = Set("S", "A", "B")
    val actualNullables = ConvertToCNF.findNullables(grammar1)
    assert(expectedNullables == actualNullables)
  }
}