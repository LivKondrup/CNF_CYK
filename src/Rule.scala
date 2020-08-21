class Rule(left: String, right: Set[String]) {
  private var isChainRule = right.size == 1

  def getLeft():String = {
    return left
  }
  def getRight():Set[String] = {
    return right
  }

  @Override
  def equals(other:Rule):Boolean = {
    var allOtherRightInThis = true
    var allThisRightInOther = true
    for (s <- other.getRight()){
      if (!right.contains(s)){
        allOtherRightInThis = false
      }
    }
    for (s <- right){
      if (!other.getRight().contains(s)){
        allThisRightInOther = false
      }
    }
    return this.left == other.getLeft() && allOtherRightInThis && allThisRightInOther
  }

  @Override
  override def toString: String = {
    return left + ", " + right.toString() + " ||| "
  }
}
