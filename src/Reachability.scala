import GrammarArchitecture._
import GraphArchitecture.Graph
import ParseTreeArchitecture.{ParseTree, ParseTreeLeaf, ParseTreeNode}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Reachability {

  private def makeReachabilityTable(graph: Graph, grammar: Grammar): (Array[Array[Set[NonTerminal]]], Array[Array[mutable.HashMap[NonTerminal, (Rule, Int, Int)]]]) = {
    // convert graph to table T
    val graphTable = convertGraphToTable(graph)

    // fill reachability table
    val (reachabilityTable, howToParseTable) = fillReachabilityTable(graphTable, grammar)

    (reachabilityTable, howToParseTable)
  }

  def getParseTree(graph: Graph, from: Int, to: Int, grammar: Grammar): ParseTree = {
    assert(canReachBetweenNodesOfGraph(graph, from, to, grammar))

    val (_, howToParseTable) = makeReachabilityTable(graph, grammar)

    getParseTreeFromHowToParseTable(from, to, grammar.getStartVariable(), howToParseTable)

  }

  private def getParseTreeFromHowToParseTable(from:Int, to:Int, nonTerminal:NonTerminal, howToParseTable:Array[Array[mutable.HashMap[NonTerminal, (Rule, Int, Int)]]]): ParseTree = {
    val (rule, intermediateNode, length) = howToParseTable(from)(to)(nonTerminal)
    // figure out which case we are in
    rule.getRight().size match {
      case 1 => // the size of the right hand side is one means that it is a terminal rule (the grammar is in CNF)
        ParseTreeNode(nonTerminal, ListBuffer(ParseTreeLeaf(rule.getRight().head)))
      case 2 =>   // There is an intermediate node
        val leftTreeNonTerminal = NonTerminal(rule.getRight().head.getName())
        val rightTreeNonTerminal = NonTerminal(rule.getRight()(1).getName())
        val leftTree = getParseTreeFromHowToParseTable(from, intermediateNode, leftTreeNonTerminal, howToParseTable)
        val rightTree = getParseTreeFromHowToParseTable(intermediateNode, to, rightTreeNonTerminal, howToParseTable)

        ParseTreeNode(nonTerminal, ListBuffer(leftTree, rightTree))
    }
  }

  private def reachabilityBetweenNodesWithNonTerminal(reachabilityTable: Array[Array[Set[NonTerminal]]], from: Int, to: Int, nonTerminal: NonTerminal): Boolean = {
    reachabilityTable(from)(to).contains(nonTerminal)
  }

  def canReachBetweenNodesOfGraph(graph:Graph, from:Int, to:Int, grammar: Grammar): Boolean = {
    // get reachability table
    val (reachabilityTable, howToParseTable) = makeReachabilityTable(graph, grammar)

    // if starting variable of grammar is in T[from,to], return true - otherwise false
    reachabilityBetweenNodesWithNonTerminal(reachabilityTable, from, to, grammar.getStartVariable())
  }

  private def fillReachabilityTable(graphTable: Array[Array[Set[Terminal]]], grammar: Grammar): (Array[Array[Set[NonTerminal]]], Array[Array[mutable.HashMap[NonTerminal, (Rule, Int, Int)]]]) = {
    val howToParseTable = makeEmptyHowToParseTable(graphTable.length)

    val reachabilityTable = initializeReachabilityTable(graphTable, grammar, howToParseTable)
    val reachabilityTableBefore = initializeReachabilityTable(graphTable, grammar, howToParseTable)



    var reachabilityTableChanged = true
    while(reachabilityTableChanged){

      fillWithSizeOneStep(reachabilityTable, grammar, howToParseTable)

      findTwoStepReachability(reachabilityTable, grammar, howToParseTable)

      reachabilityTableChanged = false
      for(i <- reachabilityTable.indices; j <- reachabilityTable.indices){
        if(!(reachabilityTable(i) sameElements reachabilityTableBefore(i))){
          reachabilityTableChanged = true
        }
      }
      fillWithSizeOneStep(reachabilityTableBefore, grammar, howToParseTable)
      findTwoStepReachability(reachabilityTableBefore, grammar, howToParseTable)
    }
    (reachabilityTable, howToParseTable)
  }

  private def findTwoStepReachability(reachabilityTable:Array[Array[Set[NonTerminal]]], grammar: Grammar, howToParseTable: Array[Array[mutable.HashMap[NonTerminal, (Rule, Int, Int)]]]): Unit = {
    for(i <- reachabilityTable.indices){
      var reachableNodesFromI = reachabilityTable(i).map(entry => if(entry.nonEmpty){reachabilityTable(i).indexOf(entry)} else {-1}).to(ListBuffer)
      reachableNodesFromI = reachableNodesFromI.filter(i => i != -1)

      for(node <- reachableNodesFromI){
        var reachableNodesFromNode = reachabilityTable(node).map(entry => if(entry.nonEmpty){reachabilityTable(node).indexOf(entry)} else {-1}).to(ListBuffer)
        reachableNodesFromNode = reachableNodesFromNode.filter(i => i != -1)

        val iToNode = reachabilityTable(i)(node)

        for(otherNode <- reachableNodesFromNode){
          val nodeToOtherNode = reachabilityTable(node)(otherNode)
          val possiblePairs: ListBuffer[ListBuffer[NonTerminal]] = ListBuffer()
          for(x <- iToNode; y <- nodeToOtherNode){
            possiblePairs += ListBuffer(x,y)
          }

          for(rule <- grammar.getRules()){
            if(possiblePairs.contains(rule.getRight())){
              reachabilityTable(i)(otherNode) += rule.getLeft()

              val lengthToParseFirstPart = howToParseTable(i)(node)(NonTerminal(rule.getRight().head.getName()))._3
              val lengthToParseSecondPart = howToParseTable(node)(otherNode)(NonTerminal(rule.getRight()(1).getName()))._3
              val length = lengthToParseFirstPart + lengthToParseSecondPart


              if(!howToParseTable(i)(otherNode).contains(rule.getLeft()) || howToParseTable(i)(otherNode)(rule.getLeft())._3 > length){     // If there is no already known shorter way of getting from i to other node
                // update howToParseTable
                howToParseTable(i)(otherNode) += (rule.getLeft() -> (rule, node, length))
              }
            }
          }
        }
      }
    }
  }

  private def fillWithSizeOneStep(reachabilityTable:Array[Array[Set[NonTerminal]]], grammar: Grammar, howToParseTable: Array[Array[mutable.HashMap[NonTerminal, (Rule, Int, Int)]]]): Unit= {
    for(i <- reachabilityTable.indices){
      for(j <- reachabilityTable.indices){
        val alreadyFound = reachabilityTable(i)(j)
        val rulesWithElemsFromAlreadyFoundOnRightSide = grammar.getRules().filter(r => r.isChainRule() && alreadyFound.contains(NonTerminal(r.getRight().head.getName())))
        val leftSidesOfTheRules = rulesWithElemsFromAlreadyFoundOnRightSide.map(r => r.getLeft())
        reachabilityTable(i)(j) = alreadyFound.union(leftSidesOfTheRules)

        // update howToParseTable
        for(rule <- rulesWithElemsFromAlreadyFoundOnRightSide){
          howToParseTable(i)(j) += (rule.getLeft() -> (rule, -1, 1))
        }
      }
    }
  }

  private def initializeReachabilityTable(graphTable: Array[Array[Set[Terminal]]], grammar: Grammar, howToParseTable: Array[Array[mutable.HashMap[NonTerminal, (Rule, Int, Int)]]]): Array[Array[Set[NonTerminal]]] = {
    val reachabilityTable = makeEmptyReachabilityTable(graphTable.length)
    for(i <- reachabilityTable.indices){
      for(j <- reachabilityTable.indices){
        val graphTableList = graphTable(i)(j)
        val rulesWithElemsFromListOnRightSide = grammar.getRules().filter(r => r.getRight().size.equals(1) && r.getRight().head.isInstanceOf[Terminal] && graphTableList.contains(Terminal(r.getRight().head.getName())))
        val leftSidesOfTheRules = rulesWithElemsFromListOnRightSide.map(r => r.getLeft())
        reachabilityTable(i)(j) = leftSidesOfTheRules

        // update howToParseTable
        for(rule <- rulesWithElemsFromListOnRightSide){
          howToParseTable(i)(j) += (rule.getLeft() -> (rule, -1, 0))
        }
      }
    }
    return reachabilityTable
  }

  private def makeEmptyReachabilityTable(size: Int): Array[Array[Set[NonTerminal]]] = {
    val reachabilityTable = Array.ofDim[Set[NonTerminal]](size,size)
    for(i <- 0 until size; j <- 0 until size){
      reachabilityTable(i)(j) = Set()
    }
    reachabilityTable
  }

  private def makeEmptyHowToParseTable(size: Int): Array[Array[mutable.HashMap[NonTerminal, (Rule, Int, Int)]]] = {
    val howToParseTable = Array.ofDim[mutable.HashMap[NonTerminal, (Rule, Int, Int)]](size, size)
    for(i <- 0 until size; j <- 0 until size){
      howToParseTable(i)(j) = mutable.HashMap()
    }
    howToParseTable
  }

  private def convertGraphToTable(graph: Graph): Array[Array[Set[Terminal]]] = {
    val graphSize = graph.getSize()
    val graphTable = makeEmptyGraphTable(graphSize)
    val edges = graph.getEdges()

    for(e <- edges){
      graphTable(e.getFrom)(e.getTo) += e.getLetter
    }

    return graphTable
  }

  private def makeEmptyGraphTable(size:Int): Array[Array[Set[Terminal]]] = {
    val graphTable = Array.ofDim[Set[Terminal]](size, size)
    for(i <- 0 until size; j <- 0 until size){
      graphTable(i)(j) = Set()
    }
    graphTable
  }
}
