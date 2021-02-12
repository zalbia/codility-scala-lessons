package lessons._06.sorting._4
import utest._

import scala.util.Random

object TriangleTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test("example1") { check(Array(10, 2, 5, 1, 8, 20), 1) }
    test("example2") { check(Array(10, 50, 5, 1), 0) }
    test { check(Array(), 0) }
    test { check(Array(0), 0) }
    test { check(Array(0, 0), 0) }
    test { check(Array(0, 0, 0), 0) }
    test { check(Array(0, 0, 0), 0) }
    test { check(Array(5, 8, 10), 1) }
    test { check(Array(0, 1, 0), 0) }
    test { check(Array(Int.MinValue, Int.MinValue + 1, Int.MinValue), 0) }
  }

  def check(a: Array[Int], expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
