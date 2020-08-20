import java.util

import org.junit.jupiter.api.Test

class GrammarTest {
  @Test def noLambdaRulesAfterRemoveLambdaMethod(): Unit = {
    val rule1:Rule = new Rule("S", List("A", "B"))    // Creating rules for the grammar that should be converted
    val rule2:Rule = new Rule("A", List("B"))
    val rule3:Rule = new Rule("A", List("a", "A"))
    val rule4:Rule = new Rule("B", List("b"))
    val rule5:Rule = new Rule("B", List("lambda"))
    val rules: List[Rule] = List(rule1, rule2, rule3, rule4, rule5) //List of rules in the grammar
    val originalGrammar: Grammar = new Grammar(rules, "S") // The grammar

    val rule6:Rule = new Rule("S", List("A"))   // Rules of the grammar that it should be converted to
    val rule7:Rule = new Rule("S", List("B"))
    val rule8:Rule = new Rule("S", List("A", "B"))
    val rule9:Rule = new Rule("A", List("B"))
    val rule10:Rule = new Rule("A", List("a"))
    val rule11:Rule = new Rule("A", List("aA"))
    val rule12:Rule = new Rule("B", List("b"))
    val rules2: List[Rule] = List(rule6, rule7, rule8, rule9, rule10, rule11, rule12)
    val grammar2: Grammar = new Grammar(rules2, "S") // How the grammar should look after being converted

    originalGrammar.eliminateLambda()  //Converting grammar to not have lambda rules
    assert(originalGrammar.equals(grammar2))
  }
}