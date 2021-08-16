import GrammarArchitecture.{Rule, RuleElement}
import HistoryTreeArchitecture.{HistoryTree, HistoryTreeBuilder}

import scala.collection.mutable.ListBuffer

class ParseTreeConverter {


  def reverseTreeToOrigialGrammar(parseTree: DerivationTree, historyTrees: ListBuffer[HistoryTree]): DerivationTree ={
    var reversedDerivationTree = reverseRenaming(parseTree, ???)
    reversedDerivationTree = reverseChains(parseTree, historyTrees)
    reversedDerivationTree = reverseLambda(parseTree, historyTrees)
    ???
  }


  def reverseLambda(parseTree: DerivationTree, historyTrees: ListBuffer[HistoryTree]): DerivationTree = {
    ???
  }

  def reverseChains(parseTree: DerivationTree, historyTrees: ListBuffer[HistoryTree]): DerivationTree = {
    ???
  }


  def reverseRenaming(parseTree: DerivationTree, historyTreeBuilder: HistoryTreeBuilder): DerivationTree = {
    /*parseTree match {
      case DerivationTreeNode(name, children) =>
        var lefts:ListBuffer[RuleElement] = ListBuffer()
        for(child<-children){
          child match{
            case DerivationTreeNode(name, _) => lefts+=name
            case Leaf(term) => lefts += term
          }
        }
        val rule = new Rule(name, lefts)
        val prevRule = historyTreeBuilder.getPreviousRule(rule)
      case Leaf(term) => Leaf(term)
    }*/
    ???
  }




}
