import scala.{::, List}

object ConvertToCNF {
  def getGrammarOnCNF(grammar: Grammar):Unit = {
    var convertedGrammar = grammar
    convertedGrammar = eliminateLambda(convertedGrammar)
    convertedGrammar = eliminateChains(convertedGrammar)
    convertedGrammar = fixRighSides(convertedGrammar)
  }

  def eliminateLambda(grammar:Grammar):Grammar = {
    var nullable: Set[String] = findNullables(grammar)
    var rulesInNewGrammar=List[Rule]()

    return ???
  }

  def findNullables(grammar: Grammar): Set[String] = {
    return (findNullableVariables(grammar, findLambdaVariables(grammar)))
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
    return nullable
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
    return nullable
  }

  def eliminateChains(grammar: Grammar):Grammar = ???

  def fixRighSides(grammar: Grammar):Grammar = ???

}
