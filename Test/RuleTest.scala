import org.junit.jupiter.api.{BeforeEach, Test}

class RuleTest {
  @Test
  def equalsWorksCorrect(): Unit ={
    val rule1 = new Rule ("A", Set("a", "A"))
    val rule2 = new Rule ("A", Set("a", "A"))

    assert(rule1.equals(rule2))
  }

}
