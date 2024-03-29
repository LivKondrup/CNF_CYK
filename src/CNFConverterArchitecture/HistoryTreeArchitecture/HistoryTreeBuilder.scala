package CNFConverterArchitecture.HistoryTreeArchitecture

import GrammarArchitecture.{Grammar, Rule}
import ParseTreeArchitecture.ParseTree
import com.sun.org.apache.xerces.internal.impl.xpath.XPath.Step

import scala.collection.mutable.ListBuffer

class HistoryTreeBuilder() extends HistoryBuilder {
  private var historyTreesLambda = new ListBuffer[HistoryTree]
  private var historyTreesChain = new ListBuffer[HistoryTree]
  private var historyTreesRenaming = new ListBuffer[HistoryTree]

  override def init(grammar: Grammar, step: Int): Unit = {
    // create a historyTree for each of all the original rules
    val historyTree: ListBuffer[HistoryTree] = ListBuffer()

    for (rule <- grammar.getRules()) {
      historyTree.append(HistoryTreeNode(rule, Set(HistoryTreeLeaf), 0))
    }

    step match {
      case 1 => historyTreesLambda = historyTree
      case 2 => historyTreesChain = historyTree
      case 3 => historyTreesRenaming = historyTree
      case _ => ???
    }
  }

  override def ruleUpdated(oldRule: Rule, newRule: Rule, step: Int): Unit = {
    if(step == 2){
      val newTree = HistoryTreeNode(oldRule, Set(HistoryTreeNode(newRule, Set(HistoryTreeLeaf), 2)),2)
      historyTreesChain += newTree
    }
    var tree = findTreeWithRule(oldRule, step)
    if(tree.equals(HistoryTreeLeaf)){
      //???     //TODO: The tree does not exist
    }

    var newTree = createNewRule(tree, oldRule, newRule, step)

    step match {
      case 1 =>
        historyTreesLambda-=tree
        historyTreesLambda+=newTree
      case 2 =>
        historyTreesChain-=tree
        historyTreesChain+=newTree
      case 3 =>
        historyTreesRenaming-=tree
        historyTreesRenaming+=newTree
    }

  }

  def findTreeWithRule(rule: Rule, step: Int): HistoryTree = {
    var correctTree:HistoryTree = HistoryTreeLeaf
    val historyTrees = step match {
      case 1 => historyTreesLambda
      case 2 => historyTreesChain
      case 3 => historyTreesRenaming
    }
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

  def getHistoryTrees(step:Int): ListBuffer[HistoryTree] = {
    step match {
      case 1 => historyTreesLambda
      case 2 => historyTreesChain
      case 3 => historyTreesRenaming
    }
  }

  def getPreviousRule(rule: Rule, step:Int): Rule = {
    val tree = findTreeWithRule(rule, step)
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
