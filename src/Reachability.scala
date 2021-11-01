import GrammarArchitecture._

import scala.collection.mutable.ListBuffer

object Reachability {

  def reachabilityBetweenNodes(graph:Graph, from:Int, to:Int, grammar: Grammar): Boolean = {
    // convert graph to table T
    val graphTable = convertGraphToTable(graph)

    // fill table
    val reachabilityTable = buildReachabilityTable(graphTable, grammar)

    // if starting variable of grammar is in T[from,to], return true - otherwise false
    return reachabilityTable(from)(to).contains(grammar.getStartVariable())
  }

  private def buildReachabilityTable(graphArray: Array[Array[ListBuffer[Terminal]]], grammar: Grammar): Array[Array[ListBuffer[NonTerminal]]] = {
    val reachabilityTable = initializeReachabilityTable(graphArray, grammar)

    var reachabilityTableChanged = true
    while(reachabilityTableChanged){
      val reachabilityTableBefore = reachabilityTable.clone()

      fillWithSizeOneStep(reachabilityTable, grammar)

      findTwoStepReachability(reachabilityTable, grammar)

      reachabilityTableChanged = !(reachabilityTable sameElements reachabilityTableBefore)
    }
    reachabilityTable
  }

  private def findTwoStepReachability(reachabilityTable:Array[Array[ListBuffer[NonTerminal]]], grammar: Grammar): Unit = {
    for(i <- reachabilityTable.indices){
      var reachableNodesFromI = reachabilityTable(i).map(entry => if(entry.nonEmpty){reachabilityTable(i).indexOf(entry)} else {-1}).to(ListBuffer)
      reachableNodesFromI -= -1

      for(node <- reachableNodesFromI){
        var reachableNodesFromNode = reachabilityTable(node).map(entry => if(entry.nonEmpty){reachabilityTable(i).indexOf(entry)} else {-1}).to(ListBuffer)
        reachableNodesFromI -= -1

        val iToNode = reachabilityTable(i)(node)

        for(otherNode <- reachableNodesFromNode){
          val nodeToOtherNode = reachabilityTable(node)(otherNode)
          var possiblePairs: ListBuffer[ListBuffer[NonTerminal]] = ListBuffer()
          for(x <- iToNode; y <- nodeToOtherNode){
            possiblePairs += ListBuffer(x,y)
          }

          for(rule <- grammar.getRules()){
            if(possiblePairs.contains(rule.getRight())){
              reachabilityTable(i)(otherNode) += rule.getLeft()
            }
          }
        }
      }
    }
  }

  private def fillWithSizeOneStep(reachabilityTable:Array[Array[ListBuffer[NonTerminal]]], grammar: Grammar): Unit= {
    for(i <- reachabilityTable.indices){
      for(j <- reachabilityTable.indices){
        val alreadyFound = reachabilityTable(i)(j)
        val rulesWithElemsFromAlreadyFoundOnRightSide = grammar.getRules().filter(r => r.getRight().size.equals(1) && alreadyFound.contains(r.getRight().head))
        val leftSidesOfTheRules = rulesWithElemsFromAlreadyFoundOnRightSide.map(r => r.getLeft())
        reachabilityTable(i)(j) = alreadyFound ++= leftSidesOfTheRules.to(ListBuffer)
      }
    }
  }

  private def initializeReachabilityTable(graphTable: Array[Array[ListBuffer[Terminal]]], grammar: Grammar): Array[Array[ListBuffer[NonTerminal]]] = {
    val reachabilityTable = Array.ofDim[ListBuffer[NonTerminal]](graphTable.length, graphTable.length)
    for(i <- reachabilityTable.indices){
      for(j <- reachabilityTable.indices){
        val graphArrayList = graphTable(i)(j)
        val rulesWithElemsFromListOnRightSide = grammar.getRules().filter(r => r.getRight().size.equals(1) && graphArrayList.contains(r.getRight().head))
        val leftSidesOfTheRules = rulesWithElemsFromListOnRightSide.map(r => r.getLeft())
        reachabilityTable(i)(j) = leftSidesOfTheRules.to(ListBuffer)
      }
    }
    return reachabilityTable
  }

  private def convertGraphToTable(graph: Graph): Array[Array[ListBuffer[Terminal]]] = {
    val graphSize = graph.getSize()
    val graphArray = Array.ofDim[ListBuffer[Terminal]](graphSize, graphSize)
    val edges = graph.getEdges()

    for(e <- edges){
      graphArray(e.getFrom)(e.getTo) += e.getLetter
    }

    return graphArray
  }
}
