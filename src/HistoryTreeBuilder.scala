import scala.collection.mutable.ListBuffer

class HistoryTreeBuilder(var originalGrammar: Grammar) extends RuleUpdatingBuilder {

  // maintain a list of all historyTrees
  var historyTrees = new ListBuffer[HistoryTree]
  // create a historyTree for each of all the original rules
  for (rule <- originalGrammar.getRules()) {
    historyTrees.append(HistoryTreeNode(rule, ListBuffer(Leaf), 0))
  }

  def ruleUpdated(oldRule: Rule, newRule: Rule, step: Int): Unit = {
    var tree = findTreeWithRule(oldRule)
    if(tree.equals(Leaf)){
      ???     // The tree does not exist
    }
    historyTrees-=tree
    val newTree = createNewRule(tree, oldRule, newRule, step)
    historyTrees+=newTree

  }

  def findTreeWithRule(rule: Rule): HistoryTree = {
    var correctTree:HistoryTree = Leaf
    for (tree <- historyTrees) {
      if (treeContainsRule(tree, rule)) {
        correctTree = tree
      }
    }
    return correctTree
  }

  def treeContainsRule(tree: HistoryTree, rule: Rule): Boolean = {
    tree match {
      case HistoryTreeNode(treeRule, children, _) =>
        if (treeRule.equals(rule)) {
          return true
        } else {
          for (child <- children) {
            if (treeContainsRule(child, rule)) return true
          }
        }
        return false
      case Leaf => return false
    }
  }

  def createNewRule(tree: HistoryTree, oldRule: Rule, newRule: Rule, step: Int): HistoryTree = {
    tree match {
      case HistoryTreeNode(rule, children, stepOld) =>
        var newChildren = children
        if (rule.equals(oldRule)){
          val newNode = HistoryTreeNode(newRule, ListBuffer(Leaf), step)
          newChildren = newChildren.append(newNode)
        } else {
          for (child <- children) {
            newChildren.append(createNewRule(child, oldRule, newRule, step))
          }
        }
        return HistoryTreeNode(rule, newChildren, stepOld)
      case Leaf => return tree
    }
  }

  def getHistoryTrees(): ListBuffer[HistoryTree] = {
    return historyTrees
  }

}