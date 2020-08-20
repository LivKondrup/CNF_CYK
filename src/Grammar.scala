class Grammar(rules: List[Rule], start:String) {

  def ConvertToCNF():Unit = {
    eliminateLambda()
    eliminateChains()
    fixRighSides()
  }

  def eliminateLambda():Unit = {
    var nullable: Set[String] = Set()   // To maintain the nullable variables
    for (rule <- rules){
      for (symbol <- rule.getRight()){
        if (symbol.equalsIgnoreCase("lambda")){
          nullable += rule.getLeft()
        }
      }
    }
  }

  def eliminateChains():Unit = ???

  def fixRighSides():Unit = ???
}
