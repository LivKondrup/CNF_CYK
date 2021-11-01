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

    val a = Array.ofDim[String](5,5)
    println(a(1)(1))
    println(a(4)(2))
    fill(a)
    println(a(1)(1))
    println(a(4)(2))

    for(i <- a){
      println(a)
    }
  }

  def fill(array:Array[Array[String]]): Unit = {
    array(1)(1) = "a"
    array(4)(2) = "b"
  }
}
