package lessons._10.prime_and_composite_numbers._2

import utest._

import scala.util.Random

object FlagsTests extends TestSuite {
  val random = new Random()
  val f      = Solution.solution _

  val tests = Tests {
    val example = Array(1, 5, 3, 4, 3, 4, 1, 2, 3, 4, 6, 2)
    test("example")(check(example, 3))
    test(check(Array(0), 0))
    test(check(Array(0, 0), 0))
    test(check(Array(0, 0, 0), 0))
    test(check(Array(0, 1, 0), 1))
    test(check(Array(0, 1, 0, 1, 0), 2))
    test(check(Array(0, 1, 0, 1, 0, 1, 0), 2))
    test(check(Array(0, 1, 0, 0, 1, 0), 2))
    test(check(Array(0, 1, 0, 0, 1, 0, 0, 1, 0), 3))
    test(check(Array(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0), 3))
    test(check(Array(0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0), 4))
    val min     = 1
    val max     = 1000000000
    val maxN    = 400000
  }

  def check(a: Array[Int], expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
