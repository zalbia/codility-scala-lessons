package lessons._02.arrays._2

import scala.collection.mutable

// N is an odd integer within the range [1..1,000,000];
// each element of array A is an integer within the range [1..1,000,000,000];
// all but one of the values in A occur an even number of times.
object Solution {
  // what a load of crap for interviews...
  // have to know/remember some niche XOR tricks
  // allocates a reducer
  def solution(a: Array[Int]): Int = // 100% | T: O(n) | S: O(1)
    a.reduce(_ ^ _)
}

object Alternative1 {
  // sets are slow, but maybe not as slow as doing this O(n^2)?
  def solution(a: Array[Int]): Int = { // 100% | time: O(n) | space: O(n)
    val candidates = mutable.Set[Int]()
    a.foreach { oddInteger =>
      if (candidates(oddInteger)) candidates -= oddInteger
      else candidates += oddInteger
    }
    candidates.head // head is safe here as there is always 1 answer
  }
}

object Alternative2 {
  // Set is persistent. mutable sets are faster though
  // about as readable as the imperative solution
  def solution(a: Array[Int]): Int = // 100% | time: O(n log n) | space: O(n)
    a.foldLeft(Set.empty[Int]) { (candidates, oddInteger) =>
      if (candidates(oddInteger)) candidates - oddInteger
      else candidates + oddInteger
    }.head
}

import utest._

import scala.util.Random

object OddOccurrencesInArrayTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test { assert(f(Array(9, 3, 9, 3, 9, 7, 9)) == 7) }
    test { assert(f(Array(1)) == 1) }
    test { assert(f(Array(1, 1, 2)) == 2) }
    test { assert(f(Array(1, 2, 2)) == 1) }
    test { assert(f(Array(1, 3, 3)) == 1) }
    test { assert(f(Array(1, 2, 2, 3, 3)) == 1) }
    test { assert(f(Array(2, 2, 1, 3, 3)) == 1) }
    test { assert(f(Array(2, 1, 3, 2, 3)) == 1) }
    test { assert(f(Array.fill(999998)(1000000000) :+ 123) == 123) }
  }
}
