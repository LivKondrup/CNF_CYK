import GrammarArchitecture.{Rule, RuleElement}
import HistoryTreeArchitecture.{HistoryTree, HistoryTreeBuilder}

import scala.collection.mutable.ListBuffer

object DerivationTreeConverter {


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
    childTrees.head
  }

  private def reverseRenamingOfSubTrees(parseTree: DerivationTree, historyTreeBuilder: HistoryTreeBuilder): ListBuffer[DerivationTree] = {
    parseTree match {
      case DerivationTreeNode(name, children) =>
        // Build the rule at the root of the current tree
        var rightSide:ListBuffer[RuleElement] = ListBuffer()
        for(child<-children){
          child match{
            case DerivationTreeNode(name, _) => rightSide+=name
            case Leaf(term) => rightSide += term
          }
        }
        val rule = new Rule(name, rightSide)

        // Find the rule previous to this rule in the history trees
        val prevRule = historyTreeBuilder.getPreviousRule(rule)

        // Recursively reverse the child trees
        val childTrees:ListBuffer[DerivationTree] = ListBuffer()
        for(child<-children){
          val newChild = reverseRenamingOfSubTrees(child, historyTreeBuilder)
          childTrees ++= newChild
        }

        // If the previous rule exist (meaning it wasn't only invented for renaming)
        // Combine the current root with the converted child trees
        if (prevRule!=null){
          ListBuffer(DerivationTreeNode(name, childTrees))
        } else {    // The rule was invented for renaming and the list of reversed subtrees, should just be passed on to the parent (this part is where layers are decreased)
          childTrees
        }
      case Leaf(term) => ListBuffer(Leaf(term))
    }
  }




}
