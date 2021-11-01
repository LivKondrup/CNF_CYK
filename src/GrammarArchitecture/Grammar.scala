package GrammarArchitecture

class Grammar(rules: Set[Rule], start:NonTerminal) {
  private var nonTerminals = Set[NonTerminal]()
  rules.foreach(rule => nonTerminals += rule.getLeft())

  def hasNonTerminal(nonTerminal: NonTerminal): Boolean = {
    for (rule <- rules){
      if (rule.getLeft().equals(nonTerminal)) return true
    }
    return false
  }

  def isCNF(): Boolean = {
    return rules.forall(rule => rule.isOnCNF())
  }


  def getRules(): Set[Rule] ={
    return rules
  }

  def getStartVariable(): NonTerminal ={
    return start
  }

  def getNonterminals(): Set[NonTerminal] = {
    return nonTerminals
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

