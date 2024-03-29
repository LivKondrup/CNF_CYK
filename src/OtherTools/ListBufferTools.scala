package OtherTools

import GrammarArchitecture.{Lambda, NonTerminal}

import scala.collection.mutable.ListBuffer

object ListBufferTools {
  private def cartesianProduct(list1: ListBuffer[NonTerminal], list2: ListBuffer[NonTerminal], x:Int): ListBuffer[(ListBuffer[NonTerminal], Int)] = {
    var listBufferlist: ListBuffer[(ListBuffer[NonTerminal], Int)] = ListBuffer()
    for (nonTerm1 <- list1; nonTerm2 <- list2) {
      listBufferlist.append((ListBuffer(nonTerm1, nonTerm2), x))
    }
    listBufferlist
  }

  // Takes the parseArray and creates a list of lists of all the variables that we need to see if can be parsed at posi, posj
  def combineUsefulPairs(parseArray: Array[Array[ListBuffer[NonTerminal]]], posi: Int, posj: Int): ListBuffer[(ListBuffer[NonTerminal], Int)] = {
    var list1: ListBuffer[ListBuffer[NonTerminal]] = ListBuffer()
    for (i <- (parseArray.length - 1) until posi by -1) {
      if(parseArray(i)(posj).isEmpty){
        list1 += ListBuffer(NonTerminal(""))
      } else {
        list1 += parseArray(i)(posj)
      }
    }

    var list2: ListBuffer[ListBuffer[NonTerminal]] = ListBuffer()
    for (x <- 1 until parseArray.length - posi) { // until the bottom
      if (parseArray(posi + x)(posj + x).isEmpty){
        list2 += ListBuffer(NonTerminal(""))
      } else {
        list2 += parseArray(posi + x)(posj + x)
      }
    }
    val list3: ListBuffer[(ListBuffer[NonTerminal], Int)] = ListBuffer()
    for (x <- list1.indices) {
      val toPrepend = cartesianProduct(list1(x), list2(x), x)
      list3 ++= toPrepend
    }

    return list3
  }

}
