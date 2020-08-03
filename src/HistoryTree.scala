class HistoryTree(initialRule: Rule){
  private val top = new HistoryNode(initialRule, top, 0)
  private var nodes: Set[HistoryNode] = Set(top)

  def addChild(rule: Rule, parentRule: Rule, step: Int): Unit = {
    val parent = nodes.find(p => p.getRule() == parentRule).get     // Can throw exception if the parent is not already in the tree
    nodes = nodes + new HistoryNode(rule, parent, step)
  }
}
