package lessons._06.sorting._2
import utest._

import scala.util.Random

object MaxProductOfThreeTests extends TestSuite {
  val random = new Random()
  val f      = Solution.solution _

  val tests = Tests {
    test("example")(check(Array(-3, 1, 2, -2, 5, 6), 60))
    test(check(Array(0, 0, 0), 0))
    test(check(Array(0, 0, 1), 0))
    test(check(Array(1, 1, 1), 1))
    test(check(Array(1, 1, 1), 1))
    test(check(Array(-9, -8, 0, 0, 0, 0, 1, 5, 10), 720))
    val maxResult    = 1000000000
    val maxN         = 100000
    val max          = 1000
    def extremeLow   = Array(-max, -max, -999)
    def randomFiller = Array.fill(maxN - 6)(random.nextInt(999))
    def extremeHigh  = Array(998, 999, max)
    test(check(Array(-max, -max, 0, 4, 5, 6, 9, 555, max), maxResult))
    test(check(extremeLow ++ randomFiller ++ extremeHigh, maxResult))
  }

  def check(a: Array[Int], expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
