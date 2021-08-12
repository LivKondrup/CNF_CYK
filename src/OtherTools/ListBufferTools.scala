package OtherTools

import GrammarArchitecture.NonTerminal

import scala.collection.mutable.ListBuffer

object ListBufferTools {
  private def cartesianProduct(list1: ListBuffer[NonTerminal], list2: ListBuffer[NonTerminal]): ListBuffer[ListBuffer[NonTerminal]] = {
    var listBufferlist: ListBuffer[ListBuffer[NonTerminal]] = ListBuffer()
    for (nonTerm1 <- list1; nonTerm2 <- list2) {
      listBufferlist += ListBuffer(nonTerm1, nonTerm2)
    }
    listBufferlist
  }

  // Takes the parseArray and creates a list of lists of all the variables that we need to see if can be parsed at posi, posj
  def combineUsefulPairs(parseArray: Array[Array[ListBuffer[NonTerminal]]], posi: Int, posj: Int): ListBuffer[ListBuffer[NonTerminal]] = {
    var list1: ListBuffer[ListBuffer[NonTerminal]] = ListBuffer()
    for (i <- (parseArray.length - 1) until posi by -1) {
      list1 += parseArray(i)(posj)
    }

    var list2: ListBuffer[ListBuffer[NonTerminal]] = ListBuffer()
    for (x <- 1 until parseArray.length - posi) { // until the bottom
      list2 += parseArray(posi + x)(posj + x)
    }

    val list3: ListBuffer[ListBuffer[NonTerminal]] = ListBuffer()
    for (x <- list1.indices) {
      val toPrepend = cartesianProduct(list1(x), list2(x))
      list3 ++= toPrepend
    }

    return list3
  }

}
