class Grammar(rules: List[Rule], start:String) {

  def getRules(): List[Rule] ={
    return rules
  }

  def getStartVariable(): String ={
    return start
  }
}
