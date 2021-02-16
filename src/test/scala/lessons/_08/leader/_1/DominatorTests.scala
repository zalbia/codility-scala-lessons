package lessons._08.leader._1

import utest._

import scala.util.Random

object DominatorTests extends TestSuite {
  val random = new Random()
  val f      = Solution.solution _

  val tests = Tests {
    test("example")(check(Array(3, 4, 3, 2, 3, -1, 3, 3), Set(0, 2, 4, 6, 7)))
    test("example")(check(Array(6, 8, 4, 6, 8, 6, 6), Set(0, 3, 5, 6)))
    test(check(Array(), Set(-1)))
    test(check(Array(1), Set(0)))
    test(check(Array(0, 0), Set(0)))
    test(check(Array(0, 1), Set(-1)))
    test(check(Array(1, 0), Set(-1)))
    test(check(Array(0, 1, 0), Set(0, 2)))
    test(check(Array(0, 1, 1), Set(1, 2)))
    test(check(Array(1, 0, 1), Set(0, 2)))
    val maxN = 100000
    test(check(Array.fill(maxN)(Int.MinValue), Set(0)))
    test(check(Array.fill(maxN)(Int.MaxValue), Set(0)))
    test(check(Array.fill(maxN / 2)(Int.MinValue) ++ Array.fill(maxN / 2)(Int.MaxValue), Set(-1)))
  }

  def check(a: Array[Int], options: Set[Int]): Unit = {
    val result = f(a)
    assert(options.contains(result))
  }
}
