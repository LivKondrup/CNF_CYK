import scala.collection.mutable.ListBuffer

sealed abstract class HistoryTree

case object Leaf extends HistoryTree

case class HistoryTreeNode(rule: Rule, children: Set[HistoryTree], step: Int) extends HistoryTree


