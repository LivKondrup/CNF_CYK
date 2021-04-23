class Grammar(rules: Set[Rule], start:NonTerminal) {
  def isCNF(): Boolean = {
    return !rules.exists(rule => !rule.isOnCNF())
  }


  def getRules(): Set[Rule] ={
    return rules
  }

  def getStartVariable(): NonTerminal ={
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
