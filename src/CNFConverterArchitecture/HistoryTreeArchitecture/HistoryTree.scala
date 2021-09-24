package CNFConverterArchitecture.HistoryTreeArchitecture

import GrammarArchitecture.Rule

sealed abstract class HistoryTree

case object HistoryTreeLeaf extends HistoryTree

case class HistoryTreeNode(rule: Rule, children: Set[HistoryTree], step: Int) extends HistoryTree


