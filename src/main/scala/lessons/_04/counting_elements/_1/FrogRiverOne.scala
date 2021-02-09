package lessons._04.counting_elements._1

import scala.collection.mutable

object Solution {
  // N and X are integers within the range [1..100,000];
  // each element of array A is an integer within the range [1..X].
  def solution(x: Int, a: Array[Int]): Int = { // 100% | O(n)
    val spots = mutable.Set[Int]()
    var earliestTime = -1
    for (i <- a.indices if spots.size < x) {
      spots += a(i)
      if (spots.size == x) earliestTime = i
    }
    earliestTime
  }
}

import utest._

import scala.util.Random

object FrogRiverOneTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test("example1") {
      val result = f(5, Array(1, 3, 1, 4, 2, 3, 5, 4))
      assert(result == 6)
    }
    test {
      val result = f(2, Array(1))
      assert(result == -1)
    }
    test {
      val result = f(2, Array(1))
      assert(result == -1)
    }
    test("extreme") {
      val result = f(100000, (1 to 100000).toArray)
      assert(result == 99999)
    }
    test("random extreme") {
      val result = f(100000, random.shuffle[Int, IndexedSeq](1 to 100000).toArray)
      assert(result == 99999)
    }
    test("1 short of max") {
      assert(f(100000, (1 to 99999).toArray) == -1)
    }
  }
}


