package GrammarArchitecture

case class Lambda() extends RuleElement {
  override def getName(): String = ""

  override def equals(obj: Any): Boolean = {
    obj match {
      case Lambda() => true
      case _ => false
    }
  }
}
