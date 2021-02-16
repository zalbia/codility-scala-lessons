import utest._

import scala.util.Random

object SolutionTests extends TestSuite {
  val random = new Random()
  val f      = Solution.solution _

  val tests = Tests {
//    test(check(???, ???))
    val min  = Int.MinValue
    val max  = Int.MaxValue
    val maxN = 100000
  }

  def check(a: Nothing, expected: Nothing): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
