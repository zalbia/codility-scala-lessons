package lessons._06.sorting._3
import utest._

import scala.util.Random

object NumberOfDiscIntersectionsTests extends TestSuite {
  val random = new Random()
  val f      = Solution.solution _

  val tests = Tests {
    test("example")(check(Array(1, 5, 2, 1, 4, 0), 11))
    // TODO: add more tests
  }

  def check(a: Array[Int], expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
