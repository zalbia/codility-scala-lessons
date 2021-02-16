package lessons._04.counting_elements._2
import utest._

import scala.util.Random

object MaxCountersTests extends TestSuite {
  val random    = new Random()
  val solutions = Array(
    Solution.solution _,
    OopAlternative.solution _,
    FpAlternative.solution _
  )

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

  private def check(n: Int, a: Array[Int], expected: Array[Int]): Unit =
    solutions.foreach { f =>
      val maxCounters  = f(n, a).toList
      val length       = maxCounters.length
      assert(length == n)
      val expectedList = expected.toList
      assert(maxCounters == expectedList)
    }
}
