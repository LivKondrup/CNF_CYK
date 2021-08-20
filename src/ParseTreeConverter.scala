import GrammarArchitecture.{Rule, RuleElement}
import HistoryTreeArchitecture.{HistoryTree, HistoryTreeBuilder}

import scala.collection.mutable.ListBuffer

class ParseTreeConverter {


  def reverseTreeToOrigialGrammar(parseTree: DerivationTree, historyTreeBuilder: HistoryTreeBuilder): DerivationTree ={
    var reversedDerivationTree = reverseRenaming(parseTree, historyTreeBuilder)
    reversedDerivationTree = reverseChains(parseTree, historyTreeBuilder)
    reversedDerivationTree = reverseLambda(parseTree, historyTreeBuilder)
    ???
  }


  def reverseLambda(parseTree: DerivationTree, historyTreeBuilder: HistoryTreeBuilder): DerivationTree = {
    ???
  }

  def reverseChains(parseTree: DerivationTree, historyTreeBuilder: HistoryTreeBuilder): DerivationTree = {
    ???
  }

  // TODO: does not check if reversal is in correct step
  def reverseRenaming(parseTree: DerivationTree, historyTreeBuilder: HistoryTreeBuilder): DerivationTree = {
    val childTrees = reverseRenamingOfSubTrees(parseTree, historyTreeBuilder)
    parseTree match {
      case DerivationTreeNode(name, _) => DerivationTreeNode(name, childTrees)
      case Leaf(name) => return Leaf(name)
    }
  }

  private def reverseRenamingOfSubTrees(parseTree: DerivationTree, historyTreeBuilder: HistoryTreeBuilder): ListBuffer[DerivationTree] = {
    parseTree match {
      case DerivationTreeNode(name, children) =>
        var rightSide:ListBuffer[RuleElement] = ListBuffer()
        for(child<-children){
          child match{
            case DerivationTreeNode(name, _) => rightSide+=name
            case Leaf(term) => rightSide += term
          }
        }
        val rule = new Rule(name, rightSide)

        val prevRule = historyTreeBuilder.getPreviousRule(rule)

        val childTrees:ListBuffer[DerivationTree] = ListBuffer()
        for(child<-children){
          childTrees ++ reverseRenamingOfSubTrees(child, historyTreeBuilder)
        }
        if (prevRule!=null){
          return ListBuffer(DerivationTreeNode(name, childTrees))
        } else {
          return childTrees
        }
      case Leaf(term) => ListBuffer(Leaf(term))
    }
  }




}
