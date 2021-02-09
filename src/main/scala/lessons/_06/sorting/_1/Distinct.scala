package lessons._06.sorting._1

object Solution {
  def solution(a: Array[Int]) = // 100%, O(n log n), can be O(n) with set
    if (a.length <= 1) a.length
    else a.sorted.iterator.sliding(2).count(l => l.exists(_ != l.head)) + 1
}

import utest._

import scala.util.Random

object DistinctTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test("example") { check(Array(2, 1, 1, 2, 3, 1), 3) }
    test { check(Array(-1000000, 1000000), 2) }
    test { check(Array.iterate(0, 100000)(_ + 1), 100000) }
    test { check(Array.iterate(-1000000, 100000)(_ + 20), 100000) }
    test { check(Array.fill(100000)(1000000), 1) }
    test("stress test") { f(Array.fill(100000)(random.nextInt(2000000) - 1000000)) }
  }

  def check(a: Array[Int], expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
