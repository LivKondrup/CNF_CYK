
class Rule(left: NonTerminal, right: List[RuleElement]) {
  private var isChain = right.size == 1 && right(0).isInstanceOf[NonTerminal]

  def getLeft():NonTerminal = {
    return left
  }
  def getRight():List[RuleElement] = {
    return right
  }
  def isChainRule():Boolean = {
    return isChain
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

  @Override
  override def hashCode(): Int = {      // Is this hashcode good enough???
    return (left.hashCode().toString + (right.hashCode().abs/1000).toString).toInt
  }
}
