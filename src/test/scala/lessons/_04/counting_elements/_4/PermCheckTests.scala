package lessons._04.counting_elements._4
import utest._

import scala.util.Random

object PermCheckTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test(check(Array(2), 0))
    test(check(Array(4, 1, 3, 2), 1))
    test(check(Array(4, 3, 2), 0))
    test(check(Array(4, 1, 3), 0))
    test(check(Array(1, 4, 1), 0))
    test(check(Array(1, 1000000000), 0))
    test(check((1 to 100000).toArray, 1))
    test(check(Array.fill(100000)(1000000000), 0))

    def check(a: Array[Int], expected: Int): Unit = {
      val result = f(a)
      assert(result == expected)
    }
  }
}