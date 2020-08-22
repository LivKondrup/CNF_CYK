import org.junit.jupiter.api.{BeforeEach, Test}

class RuleTest {
  @Test
  def equalsWorksCorrect(): Unit ={
    val rule1 = new Rule ("S", Set("A", "B"))
    val rule2 = new Rule ("S", Set("A", "B"))

    assert(rule1 == rule2)
  }

}
