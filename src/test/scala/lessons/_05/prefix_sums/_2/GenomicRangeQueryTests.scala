package lessons._05.prefix_sums._2
import utest._

import scala.util.Random

object GenomicRangeQueryTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test("example") { check("CAGCCTA", Array(2, 5, 0), Array(4, 5, 6), Array(2,4,1)) }
    test(check("A", Array(0), Array(0), Array(1)))
    test(check("C", Array(0), Array(0), Array(2)))
    test(check("G", Array(0), Array(0), Array(3)))
    test(check("T", Array(0), Array(0), Array(4)))
    test(check("ACG", Array(0, 2), Array(2, 2), Array(1, 3)))
    test("extreme") { check("A" * 100000, Array.fill(50000)(0), (0 until 50000).toArray, Array.fill(50000)(1)) }
  }

  def check(s: String, p: Array[Int], q: Array[Int], expectedArr: Array[Int]) = {
    val result = f(s, p, q).toList
    val expected = expectedArr.toList
    assert(result == expected)
  }
}

