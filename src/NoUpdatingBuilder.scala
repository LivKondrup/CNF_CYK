class NoUpdatingBuilder extends RuleUpdatingBuilder {
  override def ruleUpdated(oldRule: Rule, newRule: Rule, step: Int): Unit = {
    return
  }
}
