class Rule(left: String, right: List[String]) {
  private var isChainRule = right.size == 1

  def getLeft():String = {
    return left
  }
  def getRight():List[String] = {
    return right
  }

  @Override
  override def equals(other:Any):Boolean = {
    other match{
      case other:Rule =>{
        return this.left.equals(other.getLeft()) && this.right.equals(other.getRight())
      }
    }
    return false
  }

  @Override
  override def toString: String = {
    return "[" + left + ", " + right.toString() + "] "
  }

  @Override
  override def hashCode(): Int = {
    val a =  right.hashCode().abs
    val b = a/1000
    val c = b.toString
    println("c: " + c)
    println(left.hashCode().toString)
    println((left.hashCode().toString + (c).toInt))
    return (left.hashCode().toString + (right.hashCode().abs/1000).toString).toInt
  }
}
