import org.junit.jupiter.api.{BeforeEach, Test}

import scala.collection.mutable.ListBuffer

class ConvertToCNFTest {
  var grammar1:Grammar = _   // A grammar for the tests to use
  @BeforeEach
  def setUp(): Unit ={
    val rule1:Rule = new Rule(new NonTerminal("S"), ListBuffer(new NonTerminal("A"), new NonTerminal("B")))    // Creating rules for the grammar that should be converted
    val rule2:Rule = new Rule(new NonTerminal("A"), ListBuffer(new NonTerminal("B")))
    val rule3:Rule = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("a"), new NonTerminal("A")))
    val rule4:Rule = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("b")))
    val rule5:Rule = new Rule(new NonTerminal("B"), ListBuffer(new Lambda()))
    val rules: Set[Rule] = Set(rule1, rule2, rule3, rule4, rule5) //ListBuffer of rules in the grammar
    grammar1 = new Grammar(rules, new NonTerminal("S")) // The grammar
  }

  @Test def grammarCorrectAfterRemoveLambdaMethod(): Unit = {
    val rule6:Rule = new Rule(new NonTerminal("S"), ListBuffer(new NonTerminal("A")))   // Rules of the grammar that grammar1 should be converted to
    val rule7:Rule = new Rule(new NonTerminal("S"), ListBuffer(new NonTerminal("B")))
    val rule8:Rule = new Rule(new NonTerminal("S"), ListBuffer(new NonTerminal("A"), new NonTerminal("B")))
    val rule9:Rule = new Rule(new NonTerminal("A"), ListBuffer(new NonTerminal("B")))
    val rule10:Rule = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("a")))
    val rule11:Rule = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("a") ,new NonTerminal("A")))
    val rule12:Rule = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("b")))

    val rules2: Set[Rule] = Set(rule6, rule7, rule8, rule9, rule10, rule11, rule12)
    val grammar2: Grammar = new Grammar(rules2, new NonTerminal("S")) // How the grammar should look after being converted
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
    val rule1 = new Rule(new NonTerminal("S"), ListBuffer(new NonTerminal("A"), new NonTerminal("B")))
    val rule2 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("a"), new NonTerminal("A")))
    val rule3 = new Rule(new NonTerminal("A"), ListBuffer(new NonTerminal("B")))
    val rule4 = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("b")))
    val rule5 = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("a"), new Terminal("b")))
    val rules1 = Set(rule1, rule2, rule3, rule4, rule5)
    val grammar = new Grammar(rules1, new NonTerminal("S"))
    val grammarConverted = ConvertToCNF.eliminateChains(grammar)

    val rule6 = new Rule(new NonTerminal("S"), ListBuffer(new NonTerminal("A"), new NonTerminal("B")))
    val rule7 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("a"), new NonTerminal("A")))
    val rule8 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("b")))
    val rule9 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("a"), new Terminal("b")))
    val rule10 = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("b")))
    val rule11 = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("a"), new Terminal("b")))
    val rules2 = Set(rule6, rule7, rule8, rule9, rule10, rule11)
    val grammarExpected = new Grammar(rules2, new NonTerminal("S"))

    assert(grammarConverted.equals(grammarExpected))

  }

  @Test
  def eliminateChainWorksWithLongerChain(): Unit ={
    val rule1 = new Rule(new NonTerminal("S"), ListBuffer(new NonTerminal("A"), new NonTerminal("B")))
    val rule2 = new Rule(new NonTerminal("S"), ListBuffer(new NonTerminal("A")))
    val rule3 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("a"), new NonTerminal("A")))
    val rule4 = new Rule(new NonTerminal("A"), ListBuffer(new NonTerminal("B")))
    val rule5 = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("b")))
    val rule6 = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("a"), new Terminal("b")))
    val rules1 = Set(rule1, rule2, rule3, rule4, rule5, rule6)
    val grammar = new Grammar(rules1, new NonTerminal("S"))
    val grammarConverted = ConvertToCNF.eliminateChains(grammar)

    val rule10 = new Rule(new NonTerminal("S"), ListBuffer(new NonTerminal("A"), new NonTerminal("B")))
    val rule11 = new Rule(new NonTerminal("S"), ListBuffer(new Terminal("a"), new NonTerminal("A")))
    val rule12 = new Rule(new NonTerminal("S"), ListBuffer(new Terminal("b")))
    val rule13 = new Rule(new NonTerminal("S"), ListBuffer(new Terminal("a"), new Terminal("b")))
    val rule14 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("a"), new NonTerminal("A")))
    val rule15 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("b")))
    val rule16 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("a"), new Terminal("b")))
    val rule17 = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("b")))
    val rule18 = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("a"), new Terminal("b")))
    val rules2 = Set(rule10, rule11, rule12, rule13, rule14, rule15, rule16, rule17, rule18)
    val grammarExpected = new Grammar(rules2, new NonTerminal("S"))

    assert(grammarConverted.equals(grammarExpected))
  }

  @Test
  def eliminateChainWorksForLoopingChains(): Unit ={
    val rule12 = new Rule(new NonTerminal("S"), ListBuffer(new NonTerminal("A"), new NonTerminal("B")))
    val rule13 = new Rule(new NonTerminal("S"), ListBuffer(new NonTerminal("A")))
    val rule14 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("a"), new NonTerminal("A")))
    val rule15 = new Rule(new NonTerminal("A"), ListBuffer(new NonTerminal("B")))
    val rule16 = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("b")))
    val rule17 = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("a"), new Terminal("b")))
    val rule18 = new Rule(new NonTerminal("B"), ListBuffer(new NonTerminal("S")))
    val rules1 = Set(rule12, rule13, rule14, rule15, rule16, rule17, rule18)
    val grammar = new Grammar(rules1, new NonTerminal("S"))
    val grammarConverted = ConvertToCNF.eliminateChains(grammar)

    val rule0 = new Rule(new NonTerminal("S"), ListBuffer(new NonTerminal("A"), new NonTerminal("B")))
    val rule1 = new Rule(new NonTerminal("S"), ListBuffer(new Terminal("a"), new NonTerminal("A")))
    val rule2 = new Rule(new NonTerminal("S"), ListBuffer(new Terminal("b")))
    val rule3 = new Rule(new NonTerminal("S"), ListBuffer(new Terminal("a"), new Terminal("b")))
    val rule4 = new Rule(new NonTerminal("A"), ListBuffer(new NonTerminal("A"), new NonTerminal("B")))
    val rule5 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("a"), new NonTerminal("A")))
    val rule6 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("b")))
    val rule7 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("a"), new Terminal("b")))
    val rule8 = new Rule(new NonTerminal("B"), ListBuffer(new NonTerminal("A"), new NonTerminal("B")))
    val rule9 = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("a"), new NonTerminal("A")))
    val rule10 = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("b")))
    val rule11 = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("a"), new Terminal("b")))
    val rules2 = Set(rule0, rule1, rule2, rule3, rule4, rule5, rule6, rule7, rule8, rule9, rule10, rule11)
    val grammarExpected = new Grammar(rules2, new NonTerminal("S"))

    assert(grammarConverted.equals(grammarExpected))
  }

  @Test
  def fixRightHandSides(): Unit = {
    val rule0 = new Rule(new NonTerminal("S"), ListBuffer(new NonTerminal("A"), new NonTerminal("B"), new NonTerminal("C")))
    val rule1 = new Rule(new NonTerminal("S"), ListBuffer(new Terminal("a"), new NonTerminal("A")))
    val rule2 = new Rule(new NonTerminal("S"), ListBuffer(new Terminal("b")))
    val rule3 = new Rule(new NonTerminal("S"), ListBuffer(new Terminal("a"), new Terminal("b")))
    val rule4 = new Rule(new NonTerminal("A"), ListBuffer(new NonTerminal("A"), new NonTerminal("B")))
    val rule5 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("a"), new NonTerminal("A")))
    val rule6 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("b")))
    val rule7 = new Rule(new NonTerminal("A"), ListBuffer(new Terminal("a"), new Terminal("b")))
    val rule8 = new Rule(new NonTerminal("B"), ListBuffer(new NonTerminal("A"), new NonTerminal("B")))
    val rule9 = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("a"), new NonTerminal("A")))
    val rule10 = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("b")))
    val rule11 = new Rule(new NonTerminal("B"), ListBuffer(new Terminal("a"), new Terminal("b")))
    val rules2 = Set(rule0, rule1, rule2, rule3, rule4, rule5, rule6, rule7, rule8, rule9, rule10, rule11)
    val grammar = ConvertToCNF.getGrammarOnCNF(new Grammar(rules2, new NonTerminal("S")))

    for (rule <- grammar.getRules()){
      val rightSide = rule.getRight()
      if (rightSide.size == 1){   // If there is only one element, that element should be a terminal
        assert(rightSide.head.isInstanceOf[Terminal])
      }
      else{   // If there is more than two elements there must be two, who are both non-terminals
        assert(rightSide.size == 2)
        assert(rightSide.head.isInstanceOf[NonTerminal])
        assert(rightSide(1).isInstanceOf[NonTerminal])
      }
    }
   }


}