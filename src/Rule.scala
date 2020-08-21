class Rule(left: String, right: Set[String]) {
  private var isChainRule = right.size == 1

  def getLeft():String = {
    return left
  }
  def getRight():Set[String] = {
    return right
  }
}
