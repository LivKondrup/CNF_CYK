class Rule(left: String, right: List[String]) {
  var isChainRule = false

  if (right.size == 1){
    isChainRule = true
  }
}
