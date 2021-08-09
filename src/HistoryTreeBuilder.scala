import scala.collection.mutable.ListBuffer

class HistoryTreeBuilder(var originalGrammar: Grammar) {
  var historyTrees = new ListBuffer[HistoryTree]
  for (rule <- originalGrammar.getRules()){
    historyTrees.append(HistoryTreeNode(rule, Top, 0))
  }

  def ruleUpdated(oldRule: Rule, newRule: Rule): Unit = {
    ???
  }

}
