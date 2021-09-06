package leetcode.find_lucky

import utest._

import scala.util.Random

object FindLuckyTests extends TestSuite {
  val random = new Random()
  val f      = Solution.findLucky _

  val tests = Tests {
    test(check(Array(2, 2, 3, 4), 2))
    test(check(Array(1, 2, 2, 3, 3, 3), 3))
    test(check(Array(2, 2, 2, 3, 3), -1))
  }

  def check(a: Array[Int], expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
