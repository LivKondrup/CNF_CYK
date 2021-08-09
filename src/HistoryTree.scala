sealed abstract class HistoryTree
case object Top extends HistoryTree
case class HistoryTreeNode(elem: Rule, parent: HistoryTree = Top, step: Int) extends HistoryTree


