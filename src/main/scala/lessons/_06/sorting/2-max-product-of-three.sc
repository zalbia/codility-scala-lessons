object Solution {
  // N is an integer within the range [3..100,000];
  // each element of array A is an integer within the range [−1,000..1,000].
  def solution(a: Array[Int]): Int = { // 100%, O(N * log(N)), can be O(N)
    val subset =
      if (a.length < 6) a
      else {
        val sorted = a.sorted
        sorted.take(3) ++ sorted.takeRight(3)
      }
    subset.permutations.map(_.toIterator.take(3).product).max
  }
}

import scala.util.Random

val random = new Random()

val f = Solution.solution _

f(Array(0, 0, 0)) // 0
f(Array(0, 0, 1)) // 0
f(Array(1, 1, 1)) // 1
f(Array(1, 1, 1)) // 1
f(Array(-3, 1, 2, -2, 5, 6)) // 60
f(Array(-9, -8, 0, 0, 0, 0, 1, 5, 10)) // 720
f(Array(-1000, -1000, 0, 4, 5, 6, 9, 555, 1000)) // 1000000000
f(Array(-1000, -1000, -999) ++ Array.fill(99994)(random.nextInt(999)) ++ Array(998, 999, 1000)) // 1000000000
