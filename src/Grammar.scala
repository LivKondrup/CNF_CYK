class Grammar(rules: Set[Rule], start:String) {

  def getRules(): Set[Rule] ={
    return rules
  }

  def getStartVariable(): String ={
    return start
  }
}
