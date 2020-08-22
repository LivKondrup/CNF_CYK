class Rule(left: String, right: Set[String]) {
  private var isChainRule = right.size == 1

  def getLeft():String = {
    return left
  }
  def getRight():Set[String] = {
    return right
  }

  @Override
  override def equals(other:Any):Boolean = {
    other match{
      case other:Rule =>{
        return this.left == other.getLeft() && this.right == other.getRight()
      }
    }
    return false
  }

  @Override
  override def toString: String = {
    return "[" + left + ", " + right.toString() + "] "
  }
}
