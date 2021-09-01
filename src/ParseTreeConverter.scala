import GrammarArchitecture.{Rule, RuleElement}
import HistoryTreeArchitecture.{HistoryTree, HistoryTreeBuilder}

import scala.collection.mutable.ListBuffer

object ParseTreeConverter {


  def reverseTreeToOrigialGrammar(parseTree: ParseTree, historyTreeBuilder: HistoryTreeBuilder): ParseTree ={
    var reversedParseTree = reverseRenaming(parseTree, historyTreeBuilder)
    reversedParseTree = reverseChains(parseTree, historyTreeBuilder)
    reversedParseTree = reverseLambda(parseTree, historyTreeBuilder)
    ???
  }


  def reverseLambda(parseTree: ParseTree, historyTreeBuilder: HistoryTreeBuilder): ParseTree = {
    ???
  }

  def reverseChains(parseTree: ParseTree, historyTreeBuilder: HistoryTreeBuilder): ParseTree = {
    ???
  }

  // TODO: does not check if reversal is in correct step
  def reverseRenaming(parseTree: ParseTree, historyTreeBuilder: HistoryTreeBuilder): ParseTree = {
    val childTrees = reverseRenamingOfSubTrees(parseTree, historyTreeBuilder)
    childTrees.head
  }

  private def reverseRenamingOfSubTrees(parseTree: ParseTree, historyTreeBuilder: HistoryTreeBuilder): ListBuffer[ParseTree] = {
    parseTree match {
      case ParseTreeNode(name, children) =>
        // Build the rule at the root of the current tree
        var rightSide:ListBuffer[RuleElement] = ListBuffer()
        for(child<-children){
          child match{
            case ParseTreeNode(name, _) => rightSide+=name
            case ParseTreeLeaf(term) => rightSide += term
          }
        }
        val rule = new Rule(name, rightSide)

        // Find the rule previous to this rule in the history trees
        val prevRule = historyTreeBuilder.getPreviousRule(rule)

        // Recursively reverse the child trees
        val childTrees:ListBuffer[ParseTree] = ListBuffer()
        for(child<-children){
          val newChild = reverseRenamingOfSubTrees(child, historyTreeBuilder)
          childTrees ++= newChild
        }

        // If the previous rule exist (meaning it wasn't only invented for renaming)
        // Combine the current root with the converted child trees
        if (prevRule!=null){
          ListBuffer(ParseTreeNode(name, childTrees))
        } else {    // The rule was invented for renaming and the list of reversed subtrees, should just be passed on to the parent (this part is where layers are decreased)
          childTrees
        }
      case ParseTreeLeaf(term) => ListBuffer(ParseTreeLeaf(term))
    }
  }




}
