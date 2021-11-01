import GrammarArchitecture.Terminal

class Edge (from:Int, to:Int, letter:Terminal) {
  def getFrom:Int = from
  def getTo:Int = to
  def getLetter:Terminal = letter
}
