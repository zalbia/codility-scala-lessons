object Solution {
  def solution = ???
}

import utest._
import scala.util.Random

object SolutionTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test { f }
  }
}
