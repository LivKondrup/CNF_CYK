package CNFConverterArchitecture

import CNFConverterArchitecture.AbstractFactory.CNFConverterFactory
import CNFConverterArchitecture.Chain.{ChainParseBuilder, ChainParses}
import CNFConverterArchitecture.Lambda.{LambdaParseBuilder, LambdaParses}
import GrammarArchitecture.{Grammar, Lambda, NonTerminal, Rule, RuleElement, Terminal}
import CNFConverterArchitecture.HistoryTreeArchitecture.{HistoryBuilder, HistoryTreeBuilder}

import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.util.control.Breaks._

class ConvertToCNF(CNFFactory: CNFConverterFactory) {
  private val buildLambdaParses = CNFFactory.createLambdaParseBuilder()
  private val buildChainParses = CNFFactory.createChainParseBuilder()
  private val ruleUpdatingBuilder = CNFFactory.createHistoryTreeBuilder()

  def getGrammarOnCNF(grammar: Grammar):Grammar = {
    ruleUpdatingBuilder.init(grammar)
    var convertedGrammar = grammar
    convertedGrammar = eliminateLambda(convertedGrammar)
    convertedGrammar = eliminateChains(convertedGrammar)
    convertedGrammar = fixRightSides(convertedGrammar)
    convertedGrammar
  }

  private def makeLambdaRules(rule: Rule, nullables: Set[RuleElement], grammar: Grammar): Set[Rule] = {
    var newRules = Set[Rule]()
    var idxOfLastNonterm = -1
    var freshNonTerm = rule.getLeft()
    breakable{
      for(i <- rule.getRight().indices){
        val elem = rule.getRight()(i)
        if(nullables.contains(elem)){
          val freshNonTerm2 = getFreshNonTerminal(new Grammar(grammar.getRules().concat(newRules), grammar.getStartVariable()))
          val restOfRule = rule.getRight().clone().slice(i+1, rule.getRight().length)
          val restOfRightSideHasNullables = restOfRule.intersect(nullables.toList).nonEmpty
          if (!restOfRightSideHasNullables){   // Current is last nullable
            val ruleElemsSinceLastNonTerm = rule.getRight().clone().slice(idxOfLastNonterm+1, i)
            val ruleElemsAfterThisNonTerm = rule.getRight().clone().slice(i+1, rule.getRight().length)
            val rule1 = new Rule(freshNonTerm, ruleElemsSinceLastNonTerm.clone() += elem)
            val rule2 = new Rule(freshNonTerm, ruleElemsSinceLastNonTerm.clone() ++= ruleElemsAfterThisNonTerm)
            if(rule1.getRight().nonEmpty){
              newRules += rule1
              ruleUpdatingBuilder.ruleUpdated(rule, rule1, 1)
            }
            if(rule2.getRight().nonEmpty){
              newRules += rule2
              ruleUpdatingBuilder.ruleUpdated(rule, rule2, 1)
            }
            idxOfLastNonterm = i
            break
          }
          val ruleElemsSinceLastNonTerm = rule.getRight().clone().slice(idxOfLastNonterm+1, i)
          val rule1 = new Rule(freshNonTerm, ruleElemsSinceLastNonTerm.clone() += elem += freshNonTerm2)
          val rule2 = new Rule(freshNonTerm, ruleElemsSinceLastNonTerm.clone() += freshNonTerm2)
          if(rule1.getRight().nonEmpty){
            newRules += rule1
            ruleUpdatingBuilder.ruleUpdated(rule, rule1, 1)
          }
          if(rule2.getRight().nonEmpty){
            newRules += rule2
            ruleUpdatingBuilder.ruleUpdated(rule, rule2, 1)
          }
          idxOfLastNonterm = i
          freshNonTerm = freshNonTerm2
        }
      }
    }
    newRules
  }

  def eliminateLambda(grammar: Grammar): Grammar = {
    var newRules = Set[Rule]()
    val originalRules = grammar.getRules()
    val nullables = findNullables(grammar)

    for(rule <- originalRules){
      if(rule.getRight().intersect(nullables.toList).nonEmpty){   // Rule has nullables
        val newRulesFromCurrentRule = makeLambdaRules(rule, nullables, new Grammar(grammar.getRules().concat(newRules), grammar.getStartVariable()))
        newRules ++= newRulesFromCurrentRule
      } else if (!(rule.getRight().head == Lambda())) {    // Rule has no nullables and is not lambda
        newRules += rule
      }
    }

    for (rule <- newRules){   // Remove lambda rules and rules that have nothing on right-hand side
      if(rule.getRight().isEmpty || rule.getRight().head == Lambda()){
        newRules -= rule
      }
    }


    new Grammar(newRules, grammar.getStartVariable())
  }

  def eliminateLambda2(grammar:Grammar):Grammar = {
    val nullable: Set[RuleElement] = findNullables(grammar)
    var rulesInNewGrammar = Set[Rule]()

    // This loop creates new rules such that all combinations of having a nullable variable or not is in the the grammar
    for (rule <- grammar.getRules()){
      val nullableVariablesIterator:Iterator[Set[RuleElement]] = nullable.filter(p => rule.getRight().contains(p)).subsets()   // Iterator of all subsets of nullable variables in the right-side of the current rule
      for (subset <- nullableVariablesIterator){    // For all subsets of relevant nullables
        val newRight = rule.getRight().filter(s => !subset.contains(s) && !s.isInstanceOf[Lambda]) // New right is the old rights, but without the variables in the current subset and without lambda
        val newRule = new Rule(rule.getLeft(), newRight)
        rulesInNewGrammar += newRule  // Adds the rule WITHOUT the variables that are nullable (and also that is not lambda)
        ruleUpdatingBuilder.ruleUpdated(rule, newRule, 1)
      }
    }
    rulesInNewGrammar = rulesInNewGrammar.filter(rule => rule.getRight().nonEmpty)  // This removes rules that does not have anything on the right-side this happens when the subset in loop above is all of the variables on the right-side

    new Grammar(rulesInNewGrammar, grammar.getStartVariable())
  }

  def findNullables(grammar: Grammar): Set[RuleElement] = {
    findNullableVariables(grammar, findLambdaVariables(grammar))
  }

  private def findLambdaVariables(grammar: Grammar):Set[RuleElement] = {
    var nullable: Set[RuleElement] = Set()   // To maintain the list of nullables
    // Finds all variables that can lead to lambda
    for (rule <- grammar.getRules()){    // Going through all rules in the grammar
      val rightIsLambda = rule.getRight().contains(Lambda())  // Does the variable lead directly to a lambda
      if (rightIsLambda){   // If all right-sides of a rule was lambda then the variable on the left is nullable and should be added to the list of nullables
        nullable += rule.getLeft()
      }
    }
    nullable
  }

  private def findNullableVariables(grammar: Grammar, lambdaVariables:Set[RuleElement]):Set[RuleElement] = {
    var nullable:Set[RuleElement] = lambdaVariables
    val listOfLambdaNonTerminals = lambdaVariables.map(variable => NonTerminal(variable.getName())).toList
    buildLambdaParses.initializeParseTrees(listOfLambdaNonTerminals.to(ListBuffer))
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
          buildLambdaParses.updateLambdaParses(rule)
        }
      }
      nullableChanged = !nullable.equals(nullableBefore)   // If the list of nullables have changed, the while-loop should keep going
    }
    nullable
  }

  private def findDerivatives(grammar: Grammar): HashMap[NonTerminal, Set[NonTerminal]] = {
    val derivatives = HashMap[NonTerminal, Set[NonTerminal]]()
    for(nonTerm <- grammar.getNonterminals()){
      var derivativesChanged = true
      var nonTermDerivable = Set[NonTerminal]()
      while(derivativesChanged){
        val derivativesSizeBefore = nonTermDerivable.size
        for(rule <- grammar.getRules()){
          if(rule.isChainRule() && (rule.getLeft().equals(nonTerm) || nonTermDerivable.contains(rule.getLeft()))){
            nonTermDerivable += NonTerminal(rule.getRight().head.getName())   // The rule is chain, so the right is just one nonTerminal
            // Build the derivations from nonTerm to rule.getRight() / NonTerminal(rule.getRight().head.getName())
            buildChainParses.newDerivation(nonTerm, NonTerminal(rule.getRight().head.getName())  , rule) // The rule is chain, so the right is just one nonTerminal
          }
        }
        derivativesChanged = !(derivativesSizeBefore == nonTermDerivable.size)
      }
      derivatives(nonTerm) = nonTermDerivable
    }
    derivatives
  }

  def eliminateChains(grammar: Grammar):Grammar = {
    val derivatives = findDerivatives(grammar)
    var rulesInConvertedGrammar = Set[Rule]()
    for(rule <- grammar.getRules()){
      if(!rule.isChainRule()){
        rulesInConvertedGrammar += rule
      }
    }
    for(nonTerm <- grammar.getNonterminals()){
      for(derivative <- derivatives(nonTerm)){
        for(rule <- grammar.getRules()){
          if(derivative == rule.getLeft() && !rule.isChainRule()){
            val newRule = new Rule(nonTerm, rule.getRight())
            rulesInConvertedGrammar += newRule
            ruleUpdatingBuilder.ruleUpdated(new Rule(nonTerm, ListBuffer(Lambda(), derivative)), newRule, 2)    // Lambda is added to be able to differentiate this from an actual rule nonterm -> derivative
          }
        }
      }
    }
    new Grammar(rulesInConvertedGrammar, grammar.getStartVariable())
  }

  /* private def eliminateChains2(grammar: Grammar):Grammar = {
    var newRules = Set[Rule]()
    for (rule <- grammar.getRules()){     // First run of finding new rules
      if(rule.isChainRule()){     // If a rule is a chain, something special needs to happen, otherwise it is just added to the new grammar
        val rightVar = rule.getRight().head   // If a rule is a chain rule, there is only one element in the right-side
        val rulesFromRightVar = grammar.getRules().filter(r => r.getLeft().equals(rightVar))    // Set of all right-sides of rules with the right-side of the chain on the left
        for (r <- rulesFromRightVar){
          val newRule = new Rule(rule.getLeft(), r.getRight())
          newRules += newRule   // Adding all rules that has the right of the chain-rule on the left
          ruleUpdatingBuilder.ruleUpdated(rule, newRule, 2)
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
          newRules -= rule      // It is removed from the list
          val rightVar = rule.getRight().head
          val rulesFromRightVar = newRules.filter(r => r.getLeft().equals(rightVar))    // The right-sides of rules are found in the updated set, not the original
          for (r <- rulesFromRightVar){
            val newRule = new Rule(rule.getLeft(), r.getRight())
            newRules += newRule   // The rules to replace the chain rule is added
            ruleUpdatingBuilder.ruleUpdated(rule, newRule, 2)
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
  } */

  private def renameTerminals(tempRules: Set[Rule], newRules: Set[Rule], grammar: Grammar): (Set[Rule], Set[Rule]) ={
    var updatedTempRules = tempRules
    var updatedNewRules = newRules
    // rule can only have terminals if it is the only one
    var noTerminalsInRuleUnlessOnlyOne = updatedTempRules.count(rule => !rule.getRight().exists(ruleElem => ruleElem.isInstanceOf[Terminal]) || rule.getRight().size.equals(1)).equals(updatedTempRules.size)
    while (!noTerminalsInRuleUnlessOnlyOne) {
      for (rule <- updatedTempRules) {
        if (rule.isOnCNF()) {
          updatedTempRules -= rule
          updatedNewRules += rule
        } else {
          // For each ruleElem: If it is a terminal, create a new rule
          val elemOption = rule.getRight().find(elem => elem.isInstanceOf[Terminal])
          if (!elemOption.isEmpty) {
            val elem = elemOption.get

            val freshNonTerminal = getFreshNonTerminal(new Grammar(updatedTempRules ++ updatedNewRules, grammar.getStartVariable())) // Create a new variable for the left side of the new rule
            val usefulTerminalRule = new Rule(freshNonTerminal, ListBuffer(elem)) // Create the new rule

            // Remove the old version of the rule
            updatedTempRules -= rule

            // Update all occurences of elem with the replacement
            val newRightsides = rule.getRight().map { case elem2 => if (elem2.equals(elem)) usefulTerminalRule.getLeft() else elem2
            }
            // Create the updated rule
            val updatedRule = new Rule(rule.getLeft(), newRightsides)
            // Add the updated rule to temp rules
            updatedTempRules += updatedRule
            updatedTempRules += usefulTerminalRule
            ruleUpdatingBuilder.ruleUpdated(rule, updatedRule, 3)
          }
        }
        noTerminalsInRuleUnlessOnlyOne = updatedTempRules.count(rule => !rule.getRight().exists(ruleElem => ruleElem.isInstanceOf[Terminal]) || rule.getRight().size.equals(1)).equals(updatedTempRules.size)
      }
    }
    return (updatedTempRules, updatedNewRules)
  }

  private def simplifyRules(tempRules: Set[Rule], newRules: Set[Rule], grammar: Grammar): (Set[Rule], Set[Rule]) = {
    var updatedTempRules = tempRules
    var updatedNewRules = newRules

    // While there are still rules that are not on CNF (Rules are moved from tempRules to newRules once they are on cnf)
    while (updatedTempRules.nonEmpty) {
      // Combine the two first variables on the right side to a new rule
      for (rule <- updatedTempRules){
        if (rule.isOnCNF()){
          updatedTempRules -= rule
          updatedNewRules += rule
        } else {
          // Create a useful rule
          val left = getFreshNonTerminal(new Grammar(updatedTempRules ++ updatedNewRules, grammar.getStartVariable()))
          val first = rule.getRight().head
          val second = rule.getRight()(1)
          val right = ListBuffer(first, second)
          val usefulRule = new Rule(left, right)

          // Remove the old version of the rule
          updatedTempRules -= rule

          // Make the new right-side
          val updatedRightSide = rule.getRight().clone()
          updatedRightSide.remove(0, 2)
          updatedRightSide.prepend(usefulRule.getLeft())

          // Make the new rule
          val newRule = new Rule(rule.getLeft(), updatedRightSide)

          // Add the updated rule to the temp rules (This could technically now be on cnf, but if it is we move it in the next iteration)
          updatedTempRules += newRule
          updatedTempRules += usefulRule
          ruleUpdatingBuilder.ruleUpdated(rule, newRule, 3)
        }
      }
    }
    (updatedTempRules, updatedNewRules)
  }

  private def getFreshNonTerminal(grammar: Grammar): NonTerminal = {
    val alphabet = ListBuffer("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
    for (letter <- alphabet){
      for(letter2 <- alphabet){
        val newNonTerminal = NonTerminal(letter+letter2)
        if (!grammar.hasNonTerminal(newNonTerminal)){
          return newNonTerminal
        }
      }
    }
    return null
  }

  def fixRightSides(grammar: Grammar):Grammar = {   // Input: a grammar with no lambdas and chain rules, Output: A grammar on CNF
    // The rules that are already on CNF
    var newRules = grammar.getRules().filter(rule => rule.isOnCNF())

    // All the rules that are still not on cnf
    var tempRules = grammar.getRules().filter(rule => !rule.isOnCNF())

    // Update all rules such that terminals become non-terminals
    val terminalsRemoved = renameTerminals(tempRules, newRules, grammar)
    tempRules = terminalsRemoved._1
    newRules = terminalsRemoved._2

    // Finish the conversion by renaming and creating new rules
    val simplifiedRules = simplifyRules(tempRules, newRules, grammar)
    tempRules = simplifiedRules._1
    newRules = simplifiedRules._2

    return new Grammar(newRules, grammar.getStartVariable())
  }

}
