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
        return other.getRules().equals(rules) && other.getStartVariable().equals(start)
      }
    }
    return false
  }
}
