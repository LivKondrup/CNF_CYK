class Graph(edges:Set[Edge]) {
  private var size = edges.size

  def getEdges(): Set[Edge] = {
    return edges
  }

  def getSize(): Int = {
    return size
  }
}
