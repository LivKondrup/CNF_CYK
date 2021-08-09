import scala.collection.mutable.ListBuffer

sealed abstract class HistoryTree
case object Leaf extends HistoryTree
case class HistoryTreeNode(elem: Rule, children: ListBuffer[HistoryTree], step: Int) extends HistoryTree


