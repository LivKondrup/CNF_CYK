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
        val a = other.getRules().subsetOf(rules)
        val b = rules.subsetOf(other.getRules())
        val c = other.getStartVariable().equals(start)
        return a && b && c
      }
    }
    return false
  }
}
