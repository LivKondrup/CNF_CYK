import scala.::
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.util.control.Breaks.break

object ConvertToCNF {
  def getGrammarOnCNF(grammar: Grammar):Grammar = {
    var convertedGrammar = grammar
    convertedGrammar = eliminateLambda(convertedGrammar)
    convertedGrammar = eliminateChains(convertedGrammar)
    convertedGrammar = fixRightSides(convertedGrammar)
    convertedGrammar
  }

  def eliminateLambda(grammar:Grammar):Grammar = {
    val nullable: Set[RuleElement] = findNullables(grammar)
    var rulesInNewGrammar = Set[Rule]()

    // This loop creates new rules such that all combinations of having a nullable variable or not is in the the grammar
    for (rule <- grammar.getRules()){
      val nullableVariablesIterator:Iterator[Set[RuleElement]] = nullable.filter(p => rule.getRight().contains(p)).subsets()   // Iterator of all subsets of nullable variables in the right-side of the rule
      for (subset <- nullableVariablesIterator){    // For all subsets of relevant nullables
        val newRight = rule.getRight().filter(s => !subset.contains(s) && s.isInstanceOf[Lambda]) // New right is the old rights, but without the variables in the current subset and without lambda
        rulesInNewGrammar += new Rule(rule.getLeft(), newRight)  // Adds the rule WITHOUT the variables that are nullable (and also that is not lambda)
      }
    }
    for (rule <- rulesInNewGrammar){    // This loop removes rules that does not have anything on the right-side
                                        // The rules happens when the subset in loop above is all of the variables on the right-side
      if (rule.getRight().isEmpty){
        rulesInNewGrammar = rulesInNewGrammar - rule
      }
    }
    new Grammar(rulesInNewGrammar, grammar.getStartVariable())
  }

  def findNullables(grammar: Grammar): Set[RuleElement] = {
    findNullableVariables(grammar, findLambdaVariables(grammar))
  }

  def findLambdaVariables(grammar: Grammar):Set[RuleElement] = {
    var nullable: Set[RuleElement] = Set()   // To maintain the list of nullables
    // Finds all variables that can lead to lambda
    for (rule <- grammar.getRules()){    // Going through all rules in the grammar
      var rightIsLambda = rule.getRight().contains("lambda")    // Does the variable lead directly to a lambda
      if (rightIsLambda){   // If all right-sides of a rule was lambda then the variable on the left is nullable and should be added to the list of nullables
        nullable += rule.getLeft()
      }
    }
    nullable
  }

  def findNullableVariables(grammar: Grammar, lambdaVariables:Set[RuleElement]):Set[RuleElement] = {
    var nullable:Set[RuleElement] = lambdaVariables
    // Finds all other nullable variables
    var nullableChanged = true    // Is true if the list of nullables have changed after the last time of going through the entire grammar
    while (nullableChanged){
      val nullableBefore = nullable   // nullableBefore is how the list looked before each iteration of the grammar
      for (rule <- grammar.getRules()){    // Going through the grammar
        var rightSidesNullable = true   // Tells if all the right-sides so far is nullable. Will only be changed if one symbol is not
        for (symbol <- rule.getRight()){    //Going through all symbols of the right-side of a rule
          if (!nullable.contains(symbol)){
            rightSidesNullable = false
          }
        }
        if (rightSidesNullable){    // If all the right-sides were nullable, then the left is nullable and should be added to nullables
          nullable += rule.getLeft()
        }
      }
      nullableChanged = !nullable.equals(nullableBefore)   // If the list of nullables have changed, the while-loop should keep going
    }
    nullable
  }

  def eliminateChains(grammar: Grammar):Grammar = {
    var newRules = Set[Rule]()
    for (rule <- grammar.getRules()){     // First run of finding new rules
      if(rule.isChainRule()){     // If a rule is a chain, something special needs to happen, otherwise it is just added to the new grammar
        val rightVar = rule.getRight().head   // If a rule is a chain rule, there is only one element in the right-side
        val rulesFromRightVar = grammar.getRules().filter(r => r.getLeft().equals(rightVar))    // Set of all right-sides of rules with the right-side of the chain on the left
        for (r <- rulesFromRightVar){
          newRules += new Rule(rule.getLeft(), r.getRight())    // Adding all rules that has the right of the chain-rule on the left
        }
      }
      else {
        newRules += rule
      }
    }
    // At this point the list of rules could still have chain rules
    // This would happen if a chain goes over multiple variables
    var chainRulesInNewRules = true
    // TODO: make sure this while-loop terminates
    while(chainRulesInNewRules){    //While there are still chain rules in the list
      for(rule <- newRules){    // Go through all of the rules
        if(rule.isChainRule()){   // If a rule is a chain rule
          newRules -= rule      // It is removed form the list
          val rightVar = rule.getRight().head
          val rulesFromRightVar = newRules.filter(r => r.getLeft().equals(rightVar))    // The right-sides of rules are found in the updated set, not the original
          for (r <- rulesFromRightVar){
            newRules += new Rule(rule.getLeft(), r.getRight())    // The rules to replace the chain rule is added
          }
        }
      }
      // At this point there could still be chain rules, this is checked and update the chainRulesInNewRules
      var stillChainRules = false
      for(rule <- newRules){
        if (rule.isChainRule()){
          stillChainRules = true
        }
      }
      chainRulesInNewRules = stillChainRules
    }
    new Grammar(newRules, grammar.getStartVariable())
  }

  def renameTerminals(tempRules: Set[Rule], newRules: Set[Rule]): (Set[Rule], Set[Rule]) ={
    var updatedTempRules = tempRules
    var updatedNewRules = newRules
    for (rule <- tempRules) {
      // For each ruleElem: If it is a terminal, create a new rule (or check if one already exists)
      for (elem <- rule.getRight()) {
        if (elem.isInstanceOf[Terminal]) {
          // check if a rule already has the single terminal on the right
          // If not, create such a rule with a fresh Variable and add it to newRules and change the current rule in tempRules to use it instead of the terminal

          // Check if there is a terminal rule with elem as the right side
          val usefulTerminalRuleAlreadyExists = updatedNewRules.exists(rule => rule.getRight().size == 1 && rule.getRight()(0).equals(elem))
          var usefulTerminalRule: Rule = null
          if (usefulTerminalRuleAlreadyExists) {
            //  The useful rule
            usefulTerminalRule = updatedNewRules.filter(rule => rule.getRight().size == 1 && rule.getRight()(0).equals(elem)).head
          }
          else {
            val freshNonTerminal = getFreshNonTerminal() // Create a new variable for the left side of the new rule
            usefulTerminalRule = new Rule(freshNonTerminal, ListBuffer(elem)) // Create the new rule
          }
          // Remove the old version of the rule
          updatedTempRules -= rule

          // Update all occurences of elem with the replacement
          val newRightsides = rule.getRight().map { case elem => usefulTerminalRule.getLeft(); case x => x }
          // Create the updated rule
          val updatedRule = new Rule(rule.getLeft(), newRightsides)
          // Add the updated rule to temp rules
          updatedTempRules += updatedRule
        }
      }
    }
    return (updatedTempRules, updatedNewRules)
  }

  def simplifyRules(tempRules: Set[Rule], newRules: Set[Rule], grammar: Grammar): (Set[Rule], Set[Rule]) = {
    var updatedTempRules = tempRules
    var updatedNewRules = newRules

    // While there are still rules that are not on CNF (Rules are moved from tempRules to newRules once they are on cnf)
    while (updatedTempRules.nonEmpty) {
      // Move rules that are on CNF to newRules
      for (rule <- updatedTempRules){
        if (rule.isOnCNF()){
          updatedTempRules -= rule
          updatedNewRules += rule
        }
      }

      // If the grammar is on CNF, stop
      if (grammar.isCNF()){
        break
      }

      // Combine the two first variables on the right side to a new rule
      for (rule <- updatedTempRules){
        // Check if the wanted rule already exists
        val usefulRuleAlreadyExists = updatedNewRules.exists(rule2 => rule2.getRight().equals(ListBuffer(rule.getRight()(0), rule.getRight()(1))))
        var usefulRule:Rule = null
        if (usefulRuleAlreadyExists){
          // Find the useful rule
          usefulRule = updatedNewRules.filter(rule2 => rule2.getRight().equals(ListBuffer(rule.getRight()(0), rule.getRight()(1)))).head
        }
        else{
          // Create a useful rule
          usefulRule = new Rule (getFreshNonTerminal(), ListBuffer(rule.getRight().head, rule.getRight()(0)))
        }
        // Remove the old version of the rule
        updatedTempRules -= rule

        // Make the new right-side
        val updatedRightSide = rule.getRight()
        updatedRightSide.remove(0,1)
        updatedRightSide.prepend(usefulRule.getLeft())

        // Make the new rule
        val newRule = new Rule(rule.getLeft(), updatedRightSide)

        // Add the updated rule to the temp rules (This could technically now be on cnf, but if it is we move it in the next iteration)
        updatedTempRules += newRule
      }
    }
    (updatedTempRules, updatedNewRules)
  }

  def getFreshNonTerminal(): NonTerminal = {
    return new NonTerminal(Random.nextString(5))
  }     // TODO: How to find a string not already in use

  def fixRightSides(grammar: Grammar):Grammar = {   // Input: a grammar with no lambdas and chain rules, Output: A grammar on CNF
    // The rules that are already on CNF
    var newRules = grammar.getRules().filter(rule => rule.isOnCNF())

    // All the rules that are still not on cnf
    var tempRules = grammar.getRules().filter(rule => !rule.isOnCNF())

    // Update all rules such that terminals become non-terminals
    val terminalsRemoved = renameTerminals(tempRules, newRules)
    tempRules = terminalsRemoved._1
    newRules = terminalsRemoved._2

    // Finish the conversion by renaming and creating new rules
    val simplifiedRules = simplifyRules(tempRules, newRules, grammar)
    tempRules = simplifiedRules._1
    newRules = simplifiedRules._2

    return new Grammar(newRules, grammar.getStartVariable())
  }

}






















