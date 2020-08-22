class Grammar(rules: Set[Rule], start:String) {

  def getRules(): Set[Rule] ={
    return rules
  }

  def getStartVariable(): String ={
    return start
  }

  @Override
  override def equals(other: Any): Boolean = {
    other match {
      case other:Grammar => {
        val setOfRulesAreTheSame = other.getRules().equals(rules)
        val startvariablesAreTheSame = other.getStartVariable().equals(start)
        return setOfRulesAreTheSame && startvariablesAreTheSame
      }
    }
    return false
  }
}
