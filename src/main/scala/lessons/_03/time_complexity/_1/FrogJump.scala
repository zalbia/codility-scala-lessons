package lessons._03.time_complexity._1

object Solution {
  // X, Y and D are integers within the range [1..1,000,000,000];
  // X ≤ Y.
  def solution(x: Int, y: Int, d: Int): Int = {
    val delta = y - x
    delta / d + (if (delta % d == 0) 0 else 1)
  }
}

import utest._

import scala.util.Random

object FrogJumpTests extends TestSuite {
  val f = Solution.solution _
  val random = new Random()

  val tests = Tests {
    test { assert(f(1, 1, 1) == 0) }
    test { assert(f(1, 1, 2) == 0) }
    test { assert(f(1, 2, 1) == 1) }
    test { assert(f(1, 1000000000, 1) == 999999999) }
    test { assert(f(1, 1000000000, 2) == 500000000) }
  }
}
