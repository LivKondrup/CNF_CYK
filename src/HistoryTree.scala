/*
 * Inspiration from:
 * https://stackoverflow.com/questions/3032771/scalas-sealed-abstract-vs-abstract-class
 * https://stackoverflow.com/questions/9129671/change-node-in-scala-case-class-tree
 */

sealed abstract class HistoryTree
case class Node(children: List[HistoryTree], value: Rule) extends HistoryTree
case class Leaf(value:Rule) extends HistoryTree

