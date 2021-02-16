package lessons._08.leader._2

import utest._

import scala.util.Random

object EquiLeaderTests extends TestSuite {
  val random = new Random()
  val f      = Solution.solution _

  val tests = Tests {
    test("example")(check(Array(4, 3, 4, 4, 4, 2), 2))
    test(check(Array(0), 0))
    test(check(Array(0, 1), 0))
    test(check(Array(0, 0), 1))
    test(check(Array(0, 1), 0))
    test(check(Array(0, 1, 0), 0))
    test(check(Array(0, 0, 0), 2))
    test(check(Array(0, 0, 0, 0), 3))
    test(check(Array(0, 0, 0, 0), 3))
    val max  = 1000000000
    val min  = -max
    val maxN = 100000
    test("extreme min")(check(Array(min, min), 1))
    test("extreme max")(check(Array(max, max), 1))
    test("extreme")(check(Array(min, max), 0))
    test("extreme large")(check(Array.fill(maxN)(max), maxN - 1))
  }

  def check(a: Array[Int], expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
