package lessons._05.prefix_sums._3

object Solution {
  // N is an integer within the range [2..100,000];
  // each element of array A is an integer within the range [−10,000..10,000].
  def solution(a: Array[Int]): Int = { // 100% | O(n)
    (minAvgSliceN(a, 2), minAvgSliceN(a, 3)) match {
      case ((slice2, slice2Start), (slice3, slice3Start)) =>
        val avg2 = slice2 * 6 / 2 // avoid floating-point and
        val avg3 = slice3 * 6 / 3 // int division using LCM
        if (avg2 < avg3) slice2Start
        else if (avg3 < avg2) slice3Start
        else math.min(slice2Start, slice3Start)
    }
  }

  // sacrifices some tuple allocation for more readability </li>
  // a.iterator minimizes array allocations </li>
  // sliding iterators are allocated instead of arrays </li>
  private def minAvgSliceN(a: Array[Int], n: Int) = {
    a.iterator.sliding(n).map(_.sum).zipWithIndex.reduceLeft { (a, b) => (a, b) match {
      case ((minSliceSum, minP), (sliceSum, p)) =>
        if (sliceSum < minSliceSum) (sliceSum, p) else (minSliceSum, minP)
    }}
  }
}
