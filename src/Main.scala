object Main {
  def main(args: Array[String]): Unit = {
    val tree: HistoryTree = Node(List(Node(List(Leaf(new Rule("A", List("B", "B")))), new Rule("A", List("B")))), new Rule("A", List("a", "b")))
  }
}
