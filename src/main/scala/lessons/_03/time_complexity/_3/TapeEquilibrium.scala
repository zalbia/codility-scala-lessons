package lessons._03.time_complexity._3

object Solution {
  // N is an integer within the range [2..100,000];
  // each element of array A is an integer within the range [-1,000..1,000].
  // array has [2..100,000] elements
  def solution(a: Array[Int]): Int = { // 100% | O(n)
    val sums = a.scan(0)(_ + _).toIterator.slice(1, a.length)
    val reverseSums = a.scanRight(0)(_ + _).toIterator.slice(1, a.length)
    sums.zip(reverseSums).map { case (a, b) => math.abs(a - b) }.min
  }
}

import utest._

import scala.util.Random

object TapeEquilibriumTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test("example") { assert(f(Array(3, 1, 2, 4, 3)) == 1) }
    test { assert(f(Array(0, 0)) == 0) }
    test { assert(f(Array(0, 1)) == 1) }
    test { assert(f(Array(0, 1)) == 1) }
    test { assert(f(Array(0, 1)) == 1) }
    test("extreme") { assert(f(Array.fill(50000)(-1000) ++ Array.fill(50000)(1000)) == 2000) }
  }
}