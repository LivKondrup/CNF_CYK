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
    val ruleIsToTwoNonterminals = getRight().size==2 && getRight()(0).isInstanceOf[NonTerminal]
    return ruleIsToASingleTerminal || ruleIsToTwoNonterminals
  }

  @Override
  override def equals(other:Any):Boolean = {
    other match{
      case other:Rule =>{
        return this.left.equals(other.getLeft()) && this.right.equals(other.getRight())
      }
    }
    return false
  }

  @Override
  override def toString: String = {
    return "[" + left.getName() + ", " + right.toString() + "] "
  }

  /*@Override
  override def hashCode(): Int = {      // Is this hashcode good enough???
    return (left.hashCode().toString + (right.hashCode().abs/1000).toString).toInt
  }*/
}
