import GrammarArchitecture.{Grammar, NonTerminal, RuleElement, Terminal}
import OtherTools.ListBufferTools
import OtherTools.ListBufferTools.combineUsefulPairs
import ParseTreeArchitecture.{ParseTree, ParseTreeLeaf, ParseTreeNode}
import org.graalvm.compiler.word.Word

import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object CYKParser {

  // This assumes that the given word CAN be parsed
  def parseAndGetParseTree(word: String, grammar: Grammar): ParseTree = {
    val parseArray = parseAndGetArray(word, grammar)
    val parseTree = getParseTreeFromParseArray(parseArray, grammar, 0, 0, word, grammar.getStartVariable())
    parseTree
  }

  private def getParseTreeFromParseArray(parseArray: Array[Array[ListBuffer[NonTerminal]]], grammar: Grammar, i: Int, j: Int, word: String, currentNonTerm: NonTerminal): ParseTree = {
    var currentNonTerminal = currentNonTerm
    if(i<word.length-1){

      val pairsThatMightDeriveThis = combineUsefulPairs(parseArray, i, j).asInstanceOf[ListBuffer[(ListBuffer[RuleElement], Int)]]
      val usefulRulesRightSide = grammar.getRules().filter(rule => rule.getLeft().equals(currentNonTerminal)).map(rule => rule.getRight())
      val usefulPairs = pairsThatMightDeriveThis.filter(pair => usefulRulesRightSide.contains(pair._1))

      val indexOfUseFulPair = usefulPairs.head._2
      val amountOfDifferentEntriesForUseFulPairs = pairsThatMightDeriveThis.map(elem => elem._2).max +1

      val posLefti = i+amountOfDifferentEntriesForUseFulPairs-indexOfUseFulPair

      val nextLeftNonTerminal = usefulPairs.head._1.head.asInstanceOf[NonTerminal]
      val nextRightNonTerminal = usefulPairs.head._1(1).asInstanceOf[NonTerminal]

      val leftTree = getParseTreeFromParseArray(parseArray, grammar, posLefti, j, word, nextLeftNonTerminal)
      val rightTree= getParseTreeFromParseArray(parseArray, grammar, i+indexOfUseFulPair+1, j+indexOfUseFulPair+1, word, nextRightNonTerminal)

      ParseTreeNode(currentNonTerminal, ListBuffer(leftTree, rightTree))
    } else {  // i==max
      ParseTreeNode(currentNonTerminal, ListBuffer(ParseTreeLeaf(Terminal(word.charAt(j).toString))))
    }
  }

  def parseAndGetArray(word:String, grammar: Grammar): Array[Array[ListBuffer[NonTerminal]]] = {
    val wordLen = word.length
    var parseArray = Array.ofDim[ListBuffer[NonTerminal]](wordLen, wordLen)

    // Fill diagonal
    parseArray = fillInitialRow(word, grammar, parseArray)

    // Dynamically fill rest
    parseArray = fillRest(grammar, parseArray, wordLen)

    parseArray
  }

  def canParse(word:String, grammar: Grammar): Boolean ={
    val parseArray = parseAndGetArray(word, grammar)
    if(parseArray(0)(0) != null && parseArray(0)(0).contains(grammar.getStartVariable())){
      return true
    }
    return false
  }

  private def fillRest(grammar: Grammar, parseArray: Array[Array[ListBuffer[NonTerminal]]], wordLen: Int): Array[Array[ListBuffer[NonTerminal]]] = {
    for(i<-(wordLen-2) to 0 by -1; j<-0 until wordLen){
      if(j<=i){
        val toSearchFor = ListBufferTools.combineUsefulPairs(parseArray, i, j).map(pair => pair._1)
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
