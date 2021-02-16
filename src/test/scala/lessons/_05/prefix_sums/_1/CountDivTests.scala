package lessons._05.prefix_sums._1
import utest._

import scala.util.Random

object CountDivTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test(check(0, 1, 1, 2))
    test(check(0, 0, 1, 1))
    test(check(6, 11, 2, 3))
    test(check(1, 10, 5, 2))
    test(check(10, 10, 17, 0))
    test(check(10, 10, 5, 1))
    test(check(11, 345, 17, 20))
    test(check(0, 2000000000, 10, 200000001))
    test(check(0, 2000000000, 1, 2000000001))
  }

  def check(a: Int, b: Int, k: Int, expected: Int): Unit = {
    val result = f(a, b, k)
    assert(result == expected)
  }
}
