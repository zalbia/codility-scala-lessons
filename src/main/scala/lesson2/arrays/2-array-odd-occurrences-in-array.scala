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

import scala.util.Random

val random = Random

val f = Solution.solution _

f(Array(9, 3, 9, 3, 9, 7, 9)) // 7
f(Array(1)) // 1
f(Array(1, 2, 2)) // 1
f(Array(1, 3, 3)) // 1
f(Array.fill(999998)(1000000000) :+ 123) // 123
