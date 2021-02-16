package lessons._03.time_complexity._3
import utest._

import scala.util.Random

object TapeEquilibriumTests extends TestSuite {
  val random = new Random()
  val f      = Solution.solution _

  val tests = Tests {
    test("example")(assert(f(Array(3, 1, 2, 4, 3)) == 1))
    test(assert(f(Array(0, 0)) == 0))
    test(assert(f(Array(0, 1)) == 1))
    test(assert(f(Array(0, 1)) == 1))
    test(assert(f(Array(0, 1)) == 1))
    test("extreme")(assert(f(Array.fill(50000)(-1000) ++ Array.fill(50000)(1000)) == 2000))
  }
}
