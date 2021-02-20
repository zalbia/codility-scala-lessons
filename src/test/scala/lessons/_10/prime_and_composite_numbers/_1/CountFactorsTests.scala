package lessons._10.prime_and_composite_numbers._1

import utest._

import scala.util.Random

object CountFactorsTests extends TestSuite {
  val random = new Random()
  val f      = Solution.solution _

  val tests = Tests {
    test("example")(check(24, 8))
    val min = 1
    test(check(min, 1))
    test(check(2, 2))
    test(check(36, 9))
    val max = Int.MaxValue
    test("extreme")(check(max, 2)) // Mersenne prime!
  }

  def check(a: Int, expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
