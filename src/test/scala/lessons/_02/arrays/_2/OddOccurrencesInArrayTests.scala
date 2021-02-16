package lessons._02.arrays._2
import utest._

import scala.util.Random

object OddOccurrencesInArrayTests extends TestSuite {
  val random    = new Random()
  val solutions = Array(
    Solution.solution _,
    Alternative1.solution _,
    Alternative2.solution _
  )

  val tests = Tests {
    test(check(Array(9, 3, 9, 3, 9, 7, 9), 7))
    test(check(Array(1), 1))
    test(check(Array(1, 1, 2), 2))
    test(check(Array(1, 2, 2), 1))
    test(check(Array(1, 3, 3), 1))
    test(check(Array(1, 2, 2, 3, 3), 1))
    test(check(Array(2, 2, 1, 3, 3), 1))
    test(check(Array(2, 1, 3, 2, 3), 1))
    test(check(Array.fill(999998)(1000000000) :+ 123, 123))

    def check(a: Array[Int], expected: Int): Unit =
      solutions.foreach { f =>
        val result = f(a)
        assert(result == expected)
      }
  }
}
