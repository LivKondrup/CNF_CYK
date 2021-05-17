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
      case other:Grammar =>
        val a = getRules().equals(other.getRules())
        val b = getStartVariable().equals(other.getStartVariable())
        a && b
      case _ => false
    }
  }
}

