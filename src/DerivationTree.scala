type DerivationNode = (String, DerivationNode)
class DerivationTree(initialVariable: String) {
  private val initialNode = (initialVariable, initialNode)
  private var nodes: Set[DerivationNode] = Set(initialNode)

  def addChild(rule: Rule, parentRule: Rule): Unit = {
    nodes = nodes + (rule, parentRule)
  }
}
