package lessons._03.time_complexity._2

// N is an integer within the range [0..100,000];
// the elements of A are all distinct;
// each element of array A is an integer within the range [1..(N + 1)].
object Solution {
  // concise, fast, no allocations
  // requires knowledge of gaussian sum...
  def solution(arr: Array[Int]): Int = // 100% | time: O(n) | space: O(1)
    ((arr.length + 1L) * (arr.length + 2) / 2 - arr.sum).toInt
}

object Alternative {
  // allocates a set of n elements
  def solution(arr: Array[Int]): Int = // 100% | O(n * "eC")
    arr.foldLeft((1 to arr.length + 1).toSet)(_ - _).head
}

import utest._
import scala.util.Random

object PermMissingElemTests extends TestSuite {
  val random = new Random()
  val solutions = Array(Solution.solution _/*, Alternative.solution _*/)

  val tests = Tests {
    test("example") { check(Array(2,3,1,5), 4) }
    test { check(Array(), 1) }
    test { check(Array(2, 3), 1) }
    test { check(Array(1, 3), 2) }
    test { check(Array(3, 1), 2) }
    test { check(Array(1, 2), 3) }
    test { check(Array(2, 1), 3) }
    test("extreme") { check((2 to 100000).toArray, 1) }
    test("extreme-shuffle") { check(random.shuffle[Int, IndexedSeq](2 to 100000).toArray, 1) }
  }

  private def check(a: Array[Int], expected: Int): Unit =
    solutions.foreach { f =>
      val actual = f(a)
      assert(actual == expected)
    }
}
