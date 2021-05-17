import scala.collection.mutable.ListBuffer

object Main {
  def main(args: Array[String]): Unit = {
    print(new Lambda() == new Lambda())

    val a = new Rule(NonTerminal("A"), ListBuffer(NonTerminal("a"), Terminal("b")))
    val b = new Rule(NonTerminal("A"), ListBuffer(NonTerminal("a"), Terminal("b")))
    val c = Set(a, b)
    val d = Set(a, b)
    print(c.equals(d))
  }
}
