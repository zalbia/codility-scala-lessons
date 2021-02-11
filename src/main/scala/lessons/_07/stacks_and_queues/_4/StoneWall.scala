package lessons._07.stacks_and_queues._4

object Solution {
  // N is an integer within the range [1..100,000];
  // each element of array H is an integer within the range [1..1,000,000,000].
  def solution(h: Array[Int]): Int = { // O(n) | 100%
    var blockCount = 0
    var stack = List.empty[Int]
    for (height <- h) {
      stack = stack.dropWhile(_ > height)
      stack match {
        case top :: _ if top == height => ()
        case _ =>
          blockCount += 1
          stack = height :: stack
      }
    }
    blockCount
  }
}

import utest._

import scala.util.Random

object StoneWallTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test("example") { check(Array(8, 8, 5, 7, 9, 8, 7, 4, 8), 7) }
    test("failing case") { check(Array(2, 5, 1, 4, 6, 7, 9, 10, 1), 8) }
    test("funnelgarden-1") { check(Array(8, 8, 5, 7, 9, 8, 7, 5, 8), 6) }
    test("funnelgarden-2") { check(Array(1, 2, 3, 4, 3), 4) }
    test("funnelgarden-2") { check(Array(8, 8, 5), 2) }
    test { check(Array(1), 1) }
    test { check(Array(1, 1), 1) }
    test { check(Array(1, 2), 2) }
    test { check(Array(2, 1), 2) }
    test { check(Array(1, 2, 1), 2) }
    test { check(Array(1, 2, 2), 2) }
    test { check(Array(1, 2, 3), 3) }
    test { check(Array(2, 3, 1), 3) }
    test { check(Array(2, 1, 3), 3) }
    test { check(Array(1, 2, 3, 1), 3) }
    test { check(Array(1, 2, 3, 2), 3) }
    test { check(Array(1, 2, 3, 2), 3) }
    val maxN = 1000000000
    val maxH = 100000
    test { check(Array(1, maxH, 1), 2) }
    test("all same") {
      val num = random.nextInt(maxN)
      check(Array.fill(100000)(num + 1), 1)
    }
    test("all different") { check((1 to maxN by (maxN / maxH)).toArray, 100000) }
  }

  def check(a: Array[Int], expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}

