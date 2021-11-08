package GraphArchitecture

class Graph(edges:Set[Edge]) {
  // the nodes is a set of integers representing a node each
  // It's calculated as the union of all from and all to of the edges
  private val nodes = edges.map(edge => edge.getTo).union(edges.map(edge => edge.getTo))

  // Size is the number of edges
  private val size = nodes.size

  def getEdges(): Set[Edge] = {
    edges
  }

  def getSize(): Int = {
    size
  }

  def getNodes(): Set[Int] = {
    nodes
  }
}
