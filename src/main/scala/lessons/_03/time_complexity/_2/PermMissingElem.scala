package lessons._03.time_complexity._2

object Solution {
  // N is an integer within the range [0..100,000];
  // the elements of A are all distinct;
  // each element of array A is an integer within the range [1..(N + 1)].
  def solution(arr: Array[Int]): Int = // 100% | O(n * eC)
    arr.foldLeft((1 to arr.length + 1).toSet)(_ - _).head
}

import utest._
import scala.util.Random

object PermMissingElemTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test("example") { assert(f(Array(2,3,1,5)) == 4) }
    test { assert(f(Array()) == 1) }
    test { assert(f(Array(2, 3)) == 1) }
    test { assert(f(Array(1, 3)) == 2) }
    test { assert(f(Array(3, 1)) == 2) }
    test { assert(f(Array(1, 2)) == 3) }
    test { assert(f(Array(2, 1)) == 3) }
    test("extreme") { assert(f((2 to 100000).toArray) == 1) }
    test("extreme-shuffle") { assert(f(random.shuffle[Int, IndexedSeq](1 to 99999).toArray) == 100000) }
  }
}