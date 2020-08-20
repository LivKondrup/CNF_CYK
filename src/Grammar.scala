class Grammar(rules: List[Rule], start:String) {

  def ConvertToCNF():Unit = {
    eliminateLambda()
    eliminateChains()
    fixRighSides()
  }

  def eliminateLambda():Unit = ???

  def eliminateChains():Unit = ???

  def fixRighSides():Unit = ???
}
