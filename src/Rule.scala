class Rule(left: String, right: List[String]) {
  private var isChainRule = right.size == 1

  def getLeft():String = {
    return left
  }
  def getRight():List[String] = {
    return right
  }
}
