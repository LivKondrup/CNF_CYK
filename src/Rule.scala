import scala.collection.mutable.ListBuffer

class Rule(left: NonTerminal, right: ListBuffer[RuleElement]) {
  private var isChain = right.size == 1 && right(0).isInstanceOf[NonTerminal]

  def getLeft():NonTerminal = {
    return left
  }
  def getRight():ListBuffer[RuleElement] = {
    return right
  }
  def isChainRule():Boolean = {
    return isChain
  }

  def isOnCNF(): Boolean = {
    val ruleIsToASingleTerminal = (getRight().size==1 && getRight()(0).isInstanceOf[Terminal])
    val ruleIsToTwoNonterminals = getRight().size==2 && getRight()(0).isInstanceOf[NonTerminal] && getRight()(1).isInstanceOf[NonTerminal]
    return ruleIsToASingleTerminal || ruleIsToTwoNonterminals
  }

  @Override
  override def equals(other:Any):Boolean = {
    other match{
      case other:Rule =>
        val a = left.equals(other.getLeft())
        val b = right.equals(other.getRight())
        a && b
      case _ => false
    }
  }

  @Override
  override def toString: String = {
    return "[" + left.getName() + ", " + right.toString() + "] "
  }

  @Override
  override def hashCode(): Int = {      // Is this hashcode good enough???
    return (left.hashCode().toString.slice(0,5) + (right.hashCode().abs).toString.slice(0,4)).toInt
  }
}
