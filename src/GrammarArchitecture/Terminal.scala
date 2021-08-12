package GrammarArchitecture

case class Terminal(name: String) extends RuleElement {
  def getName(): String = {
    return name
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case Terminal(otherName: String) => name.equals(otherName)
      case _ => false
    }
  }
}
