package lessons._03.time_complexity._1
import utest._

import scala.util.Random

object FrogJumpTests extends TestSuite {
  val f      = Solution.solution _
  val random = new Random()

  val tests = Tests {
    test(check(1, 1, 1, 0))
    test(check(1, 1, 2, 0))
    test(check(1, 2, 1, 1))
    test(check(1, 1000000000, 1, 999999999))
    test(check(1, 1000000000, 2, 500000000))
  }

  private def check(x: Int, y: Int, d: Int, expected: Int): Unit = {
    val actual = f(x, y, d)
    assert(actual == expected)
  }
}
