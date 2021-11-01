package LambdaExperiments

import java.io.{File, PrintWriter}
import java.util.Calendar

import CNFConverterArchitecture.AbstractFactory.SimpleConverter
import CNFConverterArchitecture.ConvertToCNF
import GrammarArchitecture._

import scala.collection.mutable.ListBuffer

object LambdaExperiments {

  def experimentLargeRightHandSide(): Unit = {
    val rule1 = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A"), NonTerminal("B"), NonTerminal("C"), NonTerminal("E"), NonTerminal("E"), NonTerminal("F"), NonTerminal("G"), NonTerminal("H"), NonTerminal("I"), NonTerminal("J"), NonTerminal("K"), NonTerminal("L"), NonTerminal("M"), Terminal("d"), NonTerminal("D")))
    val rule2 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    val rule3 = new Rule(NonTerminal("A"), ListBuffer(Lambda()))
    val rule4 = new Rule(NonTerminal("B"), ListBuffer(Terminal("b")))
    val rule5 = new Rule(NonTerminal("B"), ListBuffer(Lambda()))
    val rule6 = new Rule(NonTerminal("C"), ListBuffer(Terminal("c")))
    val rule7 = new Rule(NonTerminal("C"), ListBuffer(Lambda()))
    val rule8 = new Rule(NonTerminal("D"), ListBuffer(Terminal("d")))
    val rule9 = new Rule(NonTerminal("D"), ListBuffer(Lambda()))
    val rule10 = new Rule(NonTerminal("E"), ListBuffer(Terminal("d")))
    val rule11 = new Rule(NonTerminal("E"), ListBuffer(Lambda()))
    val rule12 = new Rule(NonTerminal("F"), ListBuffer(Terminal("d")))
    val rule13 = new Rule(NonTerminal("F"), ListBuffer(Lambda()))
    val rule14 = new Rule(NonTerminal("G"), ListBuffer(Terminal("d")))
    val rule15 = new Rule(NonTerminal("G"), ListBuffer(Lambda()))
    val rule16 = new Rule(NonTerminal("H"), ListBuffer(Terminal("d")))
    val rule17 = new Rule(NonTerminal("H"), ListBuffer(Lambda()))
    val rule18 = new Rule(NonTerminal("I"), ListBuffer(Terminal("d")))
    val rule19 = new Rule(NonTerminal("I"), ListBuffer(Lambda()))
    val rule20 = new Rule(NonTerminal("J"), ListBuffer(Terminal("d")))
    val rule21 = new Rule(NonTerminal("J"), ListBuffer(Lambda()))
    val rule22 = new Rule(NonTerminal("K"), ListBuffer(Terminal("d")))
    val rule23 = new Rule(NonTerminal("K"), ListBuffer(Lambda()))
    val rule24 = new Rule(NonTerminal("L"), ListBuffer(Terminal("d")))
    val rule25 = new Rule(NonTerminal("L"), ListBuffer(Lambda()))
    val rule26 = new Rule(NonTerminal("M"), ListBuffer(Terminal("d")))
    val rule27 = new Rule(NonTerminal("M"), ListBuffer(Lambda()))
    val grammar = new Grammar(Set(rule1, rule2, rule3, rule4, rule5, rule6, rule7, rule8, rule9, rule10, rule11, rule12, rule13, rule14, rule15, rule16, rule17, rule18, rule19, rule20, rule21, rule22, rule23, rule24, rule25, rule26, rule27), NonTerminal("S"))

    val converter = new ConvertToCNF(new SimpleConverter())

    val timeBefore = Calendar.getInstance.getTimeInMillis
    converter.eliminateLambda(grammar)
    val timeMiddle = Calendar.getInstance.getTimeInMillis
    converter.eliminateLambda2(grammar)
    val timeAfter = Calendar.getInstance.getTimeInMillis

    println("new: ", timeMiddle-timeBefore)
    println("old: ", timeAfter-timeMiddle)
  }

  def experimentGrowingGrammar(): Unit = {
    var rule = new Rule(NonTerminal("S"), ListBuffer(NonTerminal("A")))
    var rule2 = new Rule(NonTerminal("A"), ListBuffer(Lambda()))
    var rule3 = new Rule(NonTerminal("A"), ListBuffer(Terminal("a")))
    var grammar = new Grammar(Set(rule, rule2, rule3), NonTerminal("S"))
    val converter = new ConvertToCNF(new SimpleConverter())

    val pw = new PrintWriter(new File("newAlg.txt" ))
    val pw2 = new PrintWriter(new File("oldAlg.txt"))

    for(i <- 0 to 20){
      println(i)

      // take time for new alg
      val timeBefore = Calendar.getInstance.getTimeInMillis
      converter.eliminateLambda(grammar)
      val timeAfter = Calendar.getInstance.getTimeInMillis
      val timeElapsed = timeAfter-timeBefore
      pw.write(rule.getRight().size + "," + timeElapsed + "\n")

      // take time for old alg
      val timeBefore2 = Calendar.getInstance.getTimeInMillis
      converter.eliminateLambda2(grammar)
      val timeAfter2 = Calendar.getInstance.getTimeInMillis
      val timeElapsed2 = timeAfter2-timeBefore2
      pw2.write(rule.getRight().size + "," + timeElapsed2 + "\n")

      // Update grammar
      val fresh = getFreshNonTerminal(grammar)
      rule = new Rule(rule.getLeft(), rule.getRight().append(fresh))
      val rule4 = new Rule(fresh, ListBuffer(Lambda()))
      val rule5 = new Rule(fresh, ListBuffer(Terminal(fresh.getName().toLowerCase)))
      val newRules = grammar.getRules().union(Set(rule4, rule5))
      grammar = new Grammar(newRules, grammar.getStartVariable())
    }

    pw.close()
    pw2.close()

  }

  private def getFreshNonTerminal(grammar: Grammar): NonTerminal = {
    val alphabet = ListBuffer("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
    for (letter <- alphabet){
      for(letter2 <- alphabet){
        val newNonTerminal = NonTerminal(letter+letter2)
        if (!grammar.hasNonTerminal(newNonTerminal)){
          return newNonTerminal
        }
      }
    }
    return null
  }


  def main(args: Array[String]): Unit = {
    //experimentLargeRightHandSide()
    experimentGrowingGrammar()
  }
}
