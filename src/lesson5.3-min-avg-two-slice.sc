object Solution {
  // N is an integer within the range [2..100,000];
  // each element of array A is an integer within the range [−10,000..10,000].
  def solution(a: Array[Int]) = { // 90%
    (minAvgSliceN(a, 2), minAvgSliceN(a, 3)) match {
      case ((slice2, slice2p), (slice3, slice3p)) =>
        val avg2 = slice2 * 6 / 2 // avoid floating-point and
        val avg3 = slice3 * 6 / 3 // int division using LCM
        if (avg2 < avg3) slice2p
        else if (avg3 < avg2) slice3p
        else math.min(slice2p, slice3p)
    }
  }

  // could be faster, but this reads better to me so meh
  private def minAvgSliceN(a: Array[Int], n: Int) =
    a.iterator.sliding(n).map(_.sum).zipWithIndex.foldLeft((Int.MaxValue, -1)) {
      case ((min, minP), (a, p)) =>
        if (a < min) (a, p) else (min, minP)
    }
}

val f = Solution.solution _

f(Array(4, 2, 2, 5, 1, 5, 8)) // 1
f(Array(-10000, 10000)) // 0
f(Array(1, 2, 3)) // 0
f(Array(1, 0, 0)) // 1
f(Array(0, 1, 0)) // 0
f(Array(0, 0, 1)) // 0
f(Array(2, 1, 3)) // 0
f(Array(1, 3, 2)) // 0
f(Array(2, 3, 1)) // 1
f(Array(2, 3, 1)) // 1
