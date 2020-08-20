class DerivationTree(initialVariable: String) {
  private val initialNode = new DerivationNode(initialVariable, ???)
  private var nodes: Set[DerivationNode] = Set(initialNode)

  def addChild(value: String, parent: String): Unit = {
    val parentNode = nodes.find(p => p.getValue() == parent).get       // Can give exception if the parent is not in nodes
    nodes = nodes + new DerivationNode(value, parentNode)
  }
}
