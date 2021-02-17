package lessons._09.maximum_slice_problem._2

import utest._

import scala.util.Random

object MaxProfitTests extends TestSuite {
  val random = new Random()
  val f      = Solution.solution _

  val tests = Tests {
    test("example")(check(Array(23171, 21011, 21123, 21366, 21013, 21367), 356))
    test(check(Array(), 0))
    test(check(Array(1), 0))
    test(check(Array(0, 0), 0))
    test(check(Array(0, 1), 1))
    test(check(Array(1, 0), 0))
    test(check(Array(1, 1), 0))
    val min  = 0
    val max  = 200000
    val maxN = 400000
    test("ascending")(check((0 to 100).toArray, 100))
    test("descending")(check((100 to 1 by -1).toArray, 0))
    test("min & max")(check(Array(min, max), max))
    test("min & max")(check(Array(max, min), 0))
    test("all maxN")(check(Array.fill(maxN)(max), 0))
  }

  def check(a: Array[Int], expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
