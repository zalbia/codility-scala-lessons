object Solution {
  def solution(a: Nothing) = ???
}

import utest._

import scala.util.Random

object SolutionTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test { f }
  }

  def check(a: Nothing, expected: Nothing): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
