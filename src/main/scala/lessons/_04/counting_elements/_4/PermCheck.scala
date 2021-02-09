package lessons._04.counting_elements._4

object Solution {
  /**
   * Assumptions:
   * - N is an integer within the range [1..100,000];
   * - each element of array A is an integer within the range [1..1,000,000,000].
   */
  def solution(a: Array[Int]): Int = { // 100%
    val n = a.length
    n match {
      case 1 => if (a(0) == 1) 1 else 0
      case _ =>
        val sorted = a.sorted
        val sequenced = sorted.iterator.sliding(2)
          .forall(pair => pair(1) - pair(0) == 1)
        val is1ToN = sorted(0) == 1 && sorted(n - 1) == n
        if (sequenced && is1ToN) 1 else 0
    }
  }
}

import utest._

import scala.util.Random

object PermCheckTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test { check(Array(2), 0) }
    test { check(Array(4, 1, 3, 2), 1) }
    test { check(Array(4, 3, 2), 0) }
    test { check(Array(4, 1, 3), 0) }
    test { check(Array(1, 4, 1), 0) }
    test { check(Array(1, 1000000000), 0) }
    test { check((1 to 100000).toArray, 1) }
    test { check(Array.fill(100000)(1000000000), 0) }

    def check(a: Array[Int], expected: Int): Unit = {
      val result = f(a)
      assert(result == expected)
    }
  }
}