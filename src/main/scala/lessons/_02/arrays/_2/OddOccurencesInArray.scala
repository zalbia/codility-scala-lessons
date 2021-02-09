package lessons._02.arrays._2

object Solution {
  // N is an odd integer within the range [1..1,000,000];
  // each element of array A is an integer within the range [1..1,000,000,000];
  // all but one of the values in A occur an even number of times.
  def solution(a: Array[Int]): Int = // 100%
    a.foldLeft(Set.empty[Int]) { (candidates, a) =>
      if (candidates(a)) candidates - a
      else candidates + a
    }.head // head is safe here as there is always 1 answer
}

import utest._

import scala.util.Random

object SolutionTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test { assert(f(Array(9, 3, 9, 3, 9, 7, 9)) == 7) }
    test { assert(f(Array(1)) == 1) }
    test { assert(f(Array(1, 2, 2)) == 1) }
    test { assert(f(Array(1, 3, 3)) == 1) }
    test { assert(f(Array.fill(999998)(1000000000) :+ 123) == 123) }
  }
}
