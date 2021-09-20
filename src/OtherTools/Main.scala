package OtherTools

import GrammarArchitecture.{NonTerminal, RuleElement, Terminal}

import scala.collection.mutable.ListBuffer

object Main {
  def main(args: Array[String]): Unit = {
    var ruleElems = ListBuffer[RuleElement]()
    ruleElems += NonTerminal("A")
    ruleElems += Terminal("A")

    ruleElems.foreach(elem => println(elem == NonTerminal("A")))

  }
}
