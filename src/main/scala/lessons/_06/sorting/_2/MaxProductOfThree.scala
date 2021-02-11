package lessons._06.sorting._2

// N is an integer within the range [3..100,000];
// each element of array A is an integer within the range [−1,000..1,000].
object Solution {
  // the trick is to get only the 3 smallest & 3 biggest subsets
  def solution(a: Array[Int]): Int = { // 100% | time: O(n log n) | space: O(n)
    val subset =
      if (a.length < 6) a
      else {
        val sorted = a.sorted
        sorted.take(3) ++ sorted.takeRight(3)
      }
    subset.combinations(3).map(_.product).max
  }
}

import utest._

import scala.util.Random

object MaxProductOfThreeTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test("example") { check(Array(-3, 1, 2, -2, 5, 6), 60) }
    test { check(Array(0, 0, 0), 0) }
    test { check(Array(0, 0, 1), 0) }
    test { check(Array(1, 1, 1), 1) }
    test { check(Array(1, 1, 1), 1) }
    test { check(Array(-9, -8, 0, 0, 0, 0, 1, 5, 10), 720) }
    test { check(Array(-1000, -1000, 0, 4, 5, 6, 9, 555, 1000), 1000000000) }
    test { check(Array(-1000, -1000, -999) ++ Array.fill(99994)(random.nextInt(999)) ++ Array(998, 999, 1000), 1000000000) }
  }

  def check(a: Array[Int], expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
