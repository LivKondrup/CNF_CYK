package OtherTools

import GrammarArchitecture.{NonTerminal, RuleElement, Terminal}

import scala.collection.mutable.ListBuffer

object Main {
  def main(args: Array[String]): Unit = {
    var ruleElems = ListBuffer[RuleElement]()
    ruleElems += NonTerminal("A")
    ruleElems += Terminal("A")
    var elems2 = ruleElems
    elems2 += NonTerminal("B")
    println(ruleElems)

  }
}
