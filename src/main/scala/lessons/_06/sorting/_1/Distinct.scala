package lessons._06.sorting._1

// N is an integer within the range [0..100,000];
// each element of array A is an integer within the range [−1,000,000..1,000,000].
object Solution {
  // you _could_ map, but you'd end up having to allocate a 2-billion-element array. fuck that.
  def solution(a: Array[Int]): Int = // 100% | time: O(n log n) | space: O(n)
    if (a.length <= 1) a.length
    else 1 + a.sorted.iterator.sliding(2).count(l => l(0) != l(1))
}

import utest._

import scala.util.Random

object DistinctTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test("example") { check(Array(2, 1, 1, 2, 3, 1), 3) }
    test { check(Array(-1000000, 1000000), 2) }
    test { check(Array.iterate(0, 100000)(_ + 1), 100000) }
    test { check(Array.iterate(-1000000, 100000)(_ + 20), 100000) }
    test { check(Array.fill(100000)(1000000), 1) }
    test("stress test") { f(Array.fill(100000)(random.nextInt(2000000) - 1000000)) }
  }

  def check(a: Array[Int], expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
