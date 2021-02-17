package lessons._09.maximum_slice_problem._3

import utest._

import scala.util.Random

object MaxSliceSumTests extends TestSuite {
  val random = new Random()
  val f      = Solution.solution _

  val tests = Tests {
    test("example")(check(Array(3, 2, -6, 4, 0), 5))
    test(check(Array(0), 0))
    test(check(Array(0, 1), 1))
    test(check(Array(0, -1), 0))
    test(check(Array(0, -1), 0))
    test(check(Array(-1, 0), 0))
    test(check(Array(-1, 0, -1), 0))
    test(check(Array(-1, 0, -1, 2), 2))
    test(check(Array(-1, 0, -1, 2, 3), 5))
    val max  = 1000000
    val min  = -max
    val maxN = 1000000
    test("neg going up")(check((min to min + 100000).toArray, min + 100000))
    test(check(Array.fill(maxN)(1), maxN))
    test(check(Array.fill(maxN)(-1), -1))
    test(check(Array(min), min))
    test(check(Array(max), max))
  }

  def check(a: Array[Int], expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
