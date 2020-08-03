class DerivationNode(value: String, parent: DerivationNode) {
  def getValue(): String = {
    return value
  }

  def getParent():DerivationNode = {
    return parent
  }

}
