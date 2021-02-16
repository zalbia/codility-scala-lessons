package lessons._05.prefix_sums._3
import utest._

import scala.util.Random

object MinAvgTwoSliceTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test("example") { check(Array(4, 2, 2, 5, 1, 5, 8), 1) }
    test(check(Array(-10000, 10000), 0))
    test(check(Array(1, 0, 0), 1))
    test(check(Array(0, 1, 0), 0))
    test(check(Array(0, 0, 1), 0))
    test(check(Array(1, 2, 3), 0))
    test(check(Array(1, 3, 2), 0))
    test(check(Array(2, 1, 3), 0))
    test(check(Array(2, 3, 1), 0))
    test("extreme") { check(Array.fill(100000)(-10000), 0) }
    test("extreme-far") { check(Array.fill(99999)(10000) ++ Array(-10000), 99998) }
  }

  def check(a: Array[Int], expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
