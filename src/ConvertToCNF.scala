

object ConvertToCNF {
  def getGrammarOnCNF(grammar: Grammar):Grammar = {
    var convertedGrammar = grammar
    convertedGrammar = eliminateLambda(convertedGrammar)
    convertedGrammar = eliminateChains(convertedGrammar)
    convertedGrammar = fixRightSides(convertedGrammar)
    convertedGrammar
  }

  def eliminateLambda(grammar:Grammar):Grammar = {
    val nullable: Set[String] = findNullables(grammar)
    var rulesInNewGrammar = Set[Rule]()

    for (rule <- grammar.getRules()){
      val nullableVariablesIterator:Iterator[Set[String]] = nullable.filter(p => rule.getRight().contains(p)).subsets()   // Iterator of all subsets of nullable variables in the right-side of the rule
      for (subset <- nullableVariablesIterator){    // For all subsets of relevant nullables
        val newRight = rule.getRight().filter(s => !subset.contains(s) && !s.equalsIgnoreCase("lambda"))
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

  def findNullables(grammar: Grammar): Set[String] = {
    findNullableVariables(grammar, findLambdaVariables(grammar))
  }

  def findLambdaVariables(grammar: Grammar):Set[String] = {
    var nullable: Set[String] = Set()   // To maintain the list of nullables
    // Finds all variables that can lead to lambda
    for (rule <- grammar.getRules()){    // Going through all rules in the grammar
      var rightIsLambda = true    // Maintains if all of the symbols in the right-sides so far is lambda
      for (symbol <- rule.getRight()){    // Goes through all right-side symbols of a rule
        if (!symbol.equalsIgnoreCase("lambda")){    // If a symbol on the right is different from lambda, then the right is not only lambda
          rightIsLambda = false
        }
      }
      if (rightIsLambda){   // If all right-sides of a rule was lambda then the variable on the left is nullable and should be added ti the list of nullables
        nullable += rule.getLeft()
      }
    }
    nullable
  }

  def findNullableVariables(grammar: Grammar, lambdaVariables:Set[String]):Set[String] = {
    var nullable:Set[String] = lambdaVariables
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

  def fixRightSides(grammar: Grammar):Grammar = ???

}
