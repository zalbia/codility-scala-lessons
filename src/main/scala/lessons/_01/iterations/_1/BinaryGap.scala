package lessons._01.iterations._1

object Solution {
  // N is an integer within the range [1..2,147,483,647].
  def solution(n: Int): Int = // 100% O(log n)
    n.toBinaryString.foldLeft((0, 0)) {
      case ((biggestGap, currentGap), bit) =>
        bit match {
          case '0' => (biggestGap, currentGap + 1)
          case '1' => (if (currentGap > biggestGap) currentGap else biggestGap, 0)
          case _ => ??? // make compiler happy. don't do this in production!
        }
    }._1
}

import utest._

object BinaryGapTests extends TestSuite {
  val f = Solution.solution _

  val tests = utest.Tests {
    test { assert(f(1) == 0) }
    test { assert(f(2) == 0) }
    test { assert(f(9) == 2) }
    test { assert(f(10) == 1) }
    test { assert(f(32) == 0) }
    test { assert(f(37) == 2) }
    test { assert(f(273) == 3) }
    test { assert(f(4375) == 3) }
    test { assert(f(4096) == 0) }
    test { assert(f(Int.MaxValue) == 0) }
    test { assert(f(1073741825) == 29) }
  }
}
