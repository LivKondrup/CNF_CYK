class Grammar(rules: List[Rule], start:Rule) {

  def ConvertToCNF():Unit = {
    removeLambda()
    removeChains()
    fixRighSides()
  }

  private def removeLambda():Unit = ???

  private def removeChains():Unit = ???

  private def fixRighSides():Unit = ???
}
