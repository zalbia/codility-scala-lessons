package lessons._04.counting_elements._3
import utest._

import scala.util.Random

object MissingIntegerTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test("example") { check(Array(1, 3, 6, 4, 1, 2), 5) }
    test(check(Array(1, 2, 3), 4))
    test(check(Array(-1, -3), 1))
    test(check(Array(-100000, 100000), 1))
  }

  private def check(a: Array[Int], expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
