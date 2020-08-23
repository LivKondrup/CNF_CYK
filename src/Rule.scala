class Rule(left: String, right: List[String]) {
  private var isChain = right.size == 1 && right(0).toUpperCase().equals(right(0))

  def getLeft():String = {
    return left
  }
  def getRight():List[String] = {
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
    return "[" + left + ", " + right.toString() + "] "
  }

  @Override
  override def hashCode(): Int = {      // Is this hashcode good enough???
    return (left.hashCode().toString + (right.hashCode().abs/1000).toString).toInt
  }
}
