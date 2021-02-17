package lessons._09.maximum_slice_problem._1

import utest._

import scala.util.Random

object MaxDoubleSliceSumTests extends TestSuite {
  val random = new Random()
  val f      = Solution.solution _

  val tests = Tests {
    test("example")(check(Array(3, 2, 6, -1, 4, 5, -1, 2), 17))
    test("n = 3")(check(Array(0, 0, 0), 0))
    test("ascending")(check(Array(0, 1, 2, 3, 4, 5), 9))
    test("descending")(check(Array(5, 4, 3, 2, 1, 0), 9))
    test("4* -1s")(check(Array(-1, -1, -1, -1), 0))
    test("0s, 1s, -1s")(check(Array(0, 1, 1, -1, 1, 1, 1, 1, -1, 1, 0), 6))
    test("2 mid negatives")(check(Array(0, 1, 2, -1, 3, 4, 5, 6, 7, -1, 8, 9, 0), 44))
    test("one negative bigger")(check(Array(0, 9, 8, -1, 7, 6, 5, 4, 3, -2, 2, 1, 0), 44))
    val max  = 10000
    val min  = -max
    val maxN = 100000
    test("max n, -1")(check(Array.fill(maxN)(-1), 0))
    test("max n, min elem")(check(Array.fill(maxN)(min), 0))
    test("max n, max elem")(check(Array.fill(maxN)(max), (maxN - 3) * max))
  }

  def check(a: Array[Int], expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
