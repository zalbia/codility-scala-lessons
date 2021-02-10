package lessons._01.iterations._1

// N is an integer within the range [1..2,147,483,647].
object Solution {
  // allocates a O(log n) string
  def solution(n: Int): Int = { // 100% | time: O(log n) | space: O(log n)
    var biggestGap, currentGap = 0
    n.toBinaryString.foreach {
      case '0' => currentGap += 1
      case '1' =>
        if (currentGap > biggestGap)
          biggestGap = currentGap
        currentGap = 0
    }
    biggestGap
  }
}

object Alternative {
  // allocates short-lived tuples
  // allocates a O(log n) string
  def solution(n: Int): Int = { // 100% | time: O(log n) | space: O(log n)
    n.toBinaryString.foldLeft((0, 0)) { case ((biggestGap, currentGap), bit) =>
      bit match {
        case '0' => (biggestGap, currentGap + 1)
        case '1' => (if (currentGap > biggestGap) currentGap else biggestGap, 0)
        case _ => ??? // make compiler happy. don't do this in production!
      }
    }._1
  }
}

import utest._

object BinaryGapTests extends TestSuite {
  val solutions = Array(Solution.solution _, Alternative.solution _)

  val tests = utest.Tests {
    test { check(1, 0) }
    test { check(2, 0) }
    test { check(9, 2) }
    test { check(10, 1) }
    test { check(32, 0) }
    test { check(37, 2) }
    test { check(273, 3) }
    test { check(4375, 3) }
    test { check(4096, 0) }
    test { check(Int.MaxValue, 0) }
    test { check(1073741825, 29) }
  }

  private def check(n: Int, expected: Int): Unit = {
    solutions.foreach { f =>
      println(s"binary($n): ${n.toBinaryString}")
      val actual = f(n)
      assert(actual == expected)
    }
  }
}
