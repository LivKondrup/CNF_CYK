package CNFConverterArchitecture.HistoryTreeArchitecture

import GrammarArchitecture.{Grammar, Rule}
import ParseTreeArchitecture.ParseTree

import scala.collection.mutable.ListBuffer

class HistoryTreeBuilder() extends HistoryBuilder {
  var historyTrees = new ListBuffer[HistoryTree]

  override def init(grammar: Grammar): Unit = {
    // create a historyTree for each of all the original rules
    for (rule <- grammar.getRules()) {
      historyTrees.append(HistoryTreeNode(rule, Set(HistoryTreeLeaf), 0))
    }
  }

  override def ruleUpdated(oldRule: Rule, newRule: Rule, step: Int): Unit = {
    if(step == 2){
      val newTree = HistoryTreeNode(oldRule, Set(HistoryTreeNode(newRule, Set(HistoryTreeLeaf), 2)),2)
      historyTrees += newTree
    }
    var tree = findTreeWithRule(oldRule)
    if(tree.equals(HistoryTreeLeaf)){
      //???     //TODO: The tree does not exist
    }
    historyTrees-=tree
    var newTree = createNewRule(tree, oldRule, newRule, step)
    historyTrees+=newTree
  }

  def findTreeWithRule(rule: Rule): HistoryTree = {
    var correctTree:HistoryTree = HistoryTreeLeaf
    for (tree <- historyTrees) {
      if (treeContainsRule(tree, rule)) {
        correctTree = tree
      }
    }
    correctTree
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
      case HistoryTreeLeaf => return false
    }
  }

  private def createNewRule(tree: HistoryTree, oldRule: Rule, newRule: Rule, step: Int): HistoryTree = {
    tree match {
      case HistoryTreeNode(rule, children, stepOld) =>
        var newChildren = children
        if (rule.equals(oldRule)){
          val newNode = HistoryTreeNode(newRule, Set(HistoryTreeLeaf), step)
          newChildren += newNode
        } else {
          for (child <- children) {
            newChildren += createNewRule(child, oldRule, newRule, step)
          }
        }
        return HistoryTreeNode(rule, newChildren, stepOld)
      case HistoryTreeLeaf => return tree
    }
  }

  def getHistoryTrees(): ListBuffer[HistoryTree] = {
    historyTrees
  }

  def getPreviousRule(rule: Rule): Rule = {
    val tree = findTreeWithRule(rule)
    findPreviousRule(rule, tree)
  }

  private def findPreviousRule(ruleToFind: Rule, tree: HistoryTree): Rule = {
    tree match {
      case HistoryTreeNode(currentRule, children, _) =>
        val childIsRuleToSearchFor = children.exists(historyTree => historyTree match {
          case HistoryTreeNode(rule, _, _) => rule.equals(ruleToFind)
          case HistoryTreeLeaf => false
        })
        if (childIsRuleToSearchFor){
          return currentRule
        } else {
          for(child<- children){
            val findRuleInChildTree = findPreviousRule(ruleToFind, child)
            if (findRuleInChildTree != null){
              return findRuleInChildTree
            }
          }
        }
        return null
      case HistoryTreeLeaf => null
    }
  }


}
