package GrammarArchitecture

case class NonTerminal(name: String) extends RuleElement {
  override def getName(): String = {
    return name
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case NonTerminal(otherName: String) => name.equals(otherName)
      case _ => false
    }
  }
}
