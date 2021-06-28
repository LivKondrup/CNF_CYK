import scala.collection.mutable.ListBuffer

class Grammar(rules: Set[Rule], start:NonTerminal) {
  def hasNonTerminal(letter: NonTerminal): Boolean = {
    for (rule <- rules){
      if (rule.getLeft().equals(letter)) return true
      //if (rule.getRight().exists(term => rule.getLeft().equals(term))) return true
    }
    return false
  }

  def isCNF(): Boolean = {
    return !rules.exists(rule => !rule.isOnCNF())
  }


  def getRules(): Set[Rule] ={
    return rules
  }

  def getStartVariable(): NonTerminal ={
    return start
  }

  @Override
  override def equals(other: Any): Boolean = {
    other match {
      case other:Grammar =>
        val a = getRules().equals(other.getRules())
        val b = getStartVariable().equals(other.getStartVariable())
        a && b
      case _ => false
    }
  }

  def printGrammar(): Unit = {
    println("starting: ", start)
    for (rule <- rules){
      print(rule.getLeft().getName() + " -> ")
      for(rule2 <- rules){
        if(rule.getLeft().equals(rule2.getLeft())){
          print(rule2.getRight())
        }
      }
    }
  }
}

