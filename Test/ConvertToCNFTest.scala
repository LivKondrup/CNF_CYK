import org.junit.jupiter.api.{BeforeEach, Test}

class ConvertToCNFTest {
  var grammar1:Grammar = _   // A grammar for the tests to use
  @BeforeEach
  def setUp(): Unit ={
    val rule1:Rule = new Rule("S", List("A", "B"))    // Creating rules for the grammar that should be converted
    val rule2:Rule = new Rule("A", List("B"))
    val rule3:Rule = new Rule("A", List("a", "A"))
    val rule4:Rule = new Rule("B", List("b"))
    val rule5:Rule = new Rule("B", List("lambda"))
    val rules: Set[Rule] = Set(rule1, rule2, rule3, rule4, rule5) //List of rules in the grammar
    grammar1 = new Grammar(rules, "S") // The grammar
  }

  @Test def grammarCorrectAfterRemoveLambdaMethod(): Unit = {
    val rule6:Rule = new Rule("S", List("A"))   // Rules of the grammar that grammar1 should be converted to
    val rule7:Rule = new Rule("S", List("B"))
    val rule8:Rule = new Rule("S", List("A", "B"))
    val rule9:Rule = new Rule("A", List("B"))
    val rule10:Rule = new Rule("A", List("a"))
    val rule11:Rule = new Rule("A", List("a" ,"A"))
    val rule12:Rule = new Rule("B", List("b"))

    val rules2: Set[Rule] = Set(rule6, rule7, rule8, rule9, rule10, rule11, rule12)
    val grammar2: Grammar = new Grammar(rules2, "S") // How the grammar should look after being converted
    val grammar3 = ConvertToCNF.eliminateLambda(grammar1)    //Converting grammar to not have lambda rules

    assert(grammar3.equals(grammar2))
  }

  @Test def grammarHasChanged(): Unit = {
    val grammar2 = ConvertToCNF.eliminateLambda(grammar1)    //Converting grammar to not have lambda rules

    assert(!grammar1.equals(grammar2))
  }

  @Test
  def findAllNullableVaribles(): Unit ={
    val expectedNullables = Set("S", "A", "B")
    val actualNullables = ConvertToCNF.findNullables(grammar1)
    assert(expectedNullables == actualNullables)
  }

  @Test
  def eliminateChainWorksWithOneChainRule(): Unit ={
    val rule1 = new Rule("S", List("A", "B"))
    val rule2 = new Rule("A", List("a", "A"))
    val rule3 = new Rule("A", List("B"))
    val rule4 = new Rule("B", List("b"))
    val rule5 = new Rule("B", List("a", "b"))
    val rules1 = Set(rule1, rule2, rule3, rule4, rule5)
    val grammar = new Grammar(rules1, "S")
    val grammarConverted = ConvertToCNF.eliminateChains(grammar)

    val rule6 = new Rule("S", List("A", "B"))
    val rule7 = new Rule("A", List("a", "A"))
    val rule8 = new Rule("A", List("b"))
    val rule9 = new Rule("A", List("a", "b"))
    val rule10 = new Rule("B", List("b"))
    val rule11 = new Rule("B", List("a", "b"))
    val rules2 = Set(rule6, rule7, rule8, rule9, rule10, rule11)
    val grammarExpected = new Grammar(rules2, "S")

    grammarConverted.getRules().foreach(s => print(s))
    println("")
    grammarExpected.getRules().foreach(s => print(s))

    assert(grammarConverted.equals(grammarExpected))

  }

  @Test
  def eliminateChainWorksWithLongerChain(): Unit ={
    val rule1 = new Rule("S", List("A", "B"))
    val rule2 = new Rule("S", List("A"))
    val rule3 = new Rule("A", List("a", "A"))
    val rule4 = new Rule("A", List("B"))
    val rule5 = new Rule("B", List("b"))
    val rule6 = new Rule("B", List("a", "b"))
    val rules1 = Set(rule1, rule2, rule3, rule4, rule5, rule6)
    val grammar = new Grammar(rules1, "S")
    val grammarConverted = ConvertToCNF.eliminateChains(grammar)

    val rule10 = new Rule("S", List("A", "B"))
    val rule11 = new Rule("S", List("a", "A"))
    val rule12 = new Rule("S", List("b"))
    val rule13 = new Rule("S", List("a", "b"))
    val rule14 = new Rule("A", List("a", "A"))
    val rule15 = new Rule("A", List("b"))
    val rule16 = new Rule("A", List("a", "b"))
    val rule17 = new Rule("B", List("b"))
    val rule18 = new Rule("B", List("a", "b"))
    val rules2 = Set(rule10, rule11, rule12, rule13, rule14, rule15, rule16, rule17, rule18)
    val grammarExpected = new Grammar(rules2, "S")



    assert(grammarConverted.equals(grammarExpected))
  }
}