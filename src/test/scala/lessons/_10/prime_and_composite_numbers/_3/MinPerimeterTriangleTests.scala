package lessons._10.prime_and_composite_numbers._3

import utest._

import scala.util.Random

object MinPerimeterTriangleTests extends TestSuite {
  val random = new Random()
  val f      = Solution.solution _

  val tests = Tests {
    test(check(30, 22))
    val min = 1
    test(check(min, 4))
    test(check(4, 8))
    test(check(10, 14))
    val max = 1000000000
    test("max")(check(max, 126500))
  }

  def check(a: Int, expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
