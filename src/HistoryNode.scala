class HistoryNode(rule: Rule, parent: HistoryNode, step: Int) {
  def getRule(): Rule = {
    return rule
  }

  def getParent(): HistoryNode = {
    return parent
  }

  def getStep(): Unit ={
    return step
  }
}
