object Solution {
  // N is an integer within the range [0..100,000];
  // each element of array A fits in Int.
  def solution(a: Array[Int]): Int = { // 100% O(n*log(n))
    def isTriangular(trio: Array[Long]) = {
      val (a, b, c) = (trio(0), trio(1), trio(2))
      a + b > c && b + c > a && c + a > b
    }

    if (a.length < 3) 0
    else if (a.sorted.map(_.toLong).sliding(3).exists(isTriangular)) 1 else 0
  }
}

import scala.util.Random

val random = new Random()

val f = Solution.solution _

val example1 = Array(10, 2, 5, 1, 8, 20)
val example2 = Array(10, 50, 5, 1)

f(example1) // 1
f(example2) // 0
f(Array()) // 0
f(Array(0)) // 0
f(Array(0, 0)) // 0
f(Array(0, 0, 0)) // 0
f(Array(0, 0, 0)) // 0
f(Array(5, 8, 10)) // 1
f(Array(0, 1, 0)) // 0
f(Array(Int.MinValue, Int.MinValue + 1, Int.MinValue)) // 0
