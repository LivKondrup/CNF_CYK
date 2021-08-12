import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.mutable.ListBuffer

class CYKParser {

  def parse(word:String, grammar: Grammar): Boolean ={
    val wordLen = word.length
    var parseArray = Array.ofDim[ListBuffer[NonTerminal]](wordLen, wordLen)

    // Fill diagonal
    parseArray = fillInitialRow(word, grammar, parseArray)

    // Dynamically fill rest
    parseArray = fillRest(grammar, parseArray, wordLen)

    if(parseArray(0)(0) != null && parseArray(0)(0).contains(grammar.getStartVariable())){
      return true
    }

    return false
  }

  private def fillRest(grammar: Grammar, parseArray: Array[Array[ListBuffer[NonTerminal]]], wordLen: Int): Array[Array[ListBuffer[NonTerminal]]] = {
    for(i<-(wordLen-2) to 0 by -1; j<-0 until wordLen){
      if(j<=i){
        val toSearchFor = ListBufferTools.combineUsefulPairs(parseArray, i, j)
        val usefulRules = grammar.getRules().filter(rule => toSearchFor.contains(rule.getRight()))
        val usefulNonTerminals = usefulRules.map(rule => rule.getLeft())
        val listOfUsefulNonTerminals = ListBuffer.empty ++= usefulNonTerminals
        parseArray(i)(j) = listOfUsefulNonTerminals
      }
    }
    parseArray
  }


  private def fillInitialRow(word: String, grammar: Grammar, parseArray: Array[Array[ListBuffer[NonTerminal]]]): Array[Array[ListBuffer[NonTerminal]]] = {
    val wordLen = word.length
    for (i <- 0 until wordLen) {
      val letterToParse = word.charAt(i).toString
      // Find NonTerminal that can parse this
      val rulesWithUsefulTerminal = grammar.getRules().filter(rule => rule.getRight().contains(Terminal(letterToParse)))    // contains is good enough, since if there is a Terminal in a grammar on CNF, there will only be that one terminal
      var setOfUseFulNonTerminals = rulesWithUsefulTerminal.map(rule => rule.getLeft())
      val listOfUseFulNonTerminal = ListBuffer.empty ++= setOfUseFulNonTerminals //Creates a listBuffer from a Set

      parseArray(wordLen-1)(i) = listOfUseFulNonTerminal

    }
    parseArray
  }
}
