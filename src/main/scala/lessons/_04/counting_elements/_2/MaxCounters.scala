package lessons._04.counting_elements._2

// N and M are integers within the range [1..100,000];
// M = a.size
// each element of array A is an integer within the range [1..N + 1]

object Solution {
  // would look bloody as a fold
  def solution(n: Int, a: Array[Int]): Array[Int] = { // 100% | O(n + m)
    val counters = Array.ofDim[Int](n)
    var maxCounter = 0
    var offset = 0
    for (i <- a.indices) {
      if (a(i) <= n) {
        val counterIdx = a(i) - 1
        if (counters(counterIdx) < offset)
          counters(counterIdx) = offset + 1
        else
          counters(counterIdx) += 1

        if (counters(counterIdx) > maxCounter)
          maxCounter = counters(counterIdx)
      } else if (a(i) == n + 1)
        offset = maxCounter
    }
    counters.map(math.max(offset, _))
  }
}

import utest._

import scala.util.Random

object SolutionTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test("example") {
      check(n = 5, a = Array(3, 4, 4, 6, 1, 4, 4), expected = Array(3, 2, 2, 4, 2))
    }
    test("extreme-ones") {
      check(n = 100000, a = Array.fill(100000)(1), expected = (100000 :: List.fill(99999)(0)).toArray)
    }
    test("extreme-max-counters") {
      check(n = 100000, a = Array.fill(100000)(100001), expected = Array.fill(100000)(0))
    }
  }

  private def check(n: Int, a: Array[Int], expected: Array[Int]): Unit = {
    val maxCounters = f(n, a).toList
    val length = maxCounters.length
    assert(length == n)
    assert(maxCounters == expected.toList)
  }
}
