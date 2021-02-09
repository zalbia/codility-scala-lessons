import utest.Tests

import scala.util.{Failure, Success}

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

val f = Solution.solution _

import utest._

object SolutionTests extends TestSuite {
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

TestRunner.run(SolutionTests.tests).leaves.map(_.value).foreach {
  case Failure(exception) => println(exception)
  case Success(value) => println(value)
}
