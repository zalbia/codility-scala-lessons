package lessons._09.maximum_slice_problem._1

object Solution {
  // get prefix/suffix sum max, then find max sum of left & right slices.
  def solution(a: Array[Int]): Int = { // 100% | T: O(n) | S: O(n)
    val n     = a.length
    val left  = a.slice(1, n).scanLeft(0)((b, a) => math.max(0, a + b))
    val right = a.slice(0, n - 1).scanRight(0)((a, b) => math.max(0, b + a))
    (1 until n - 1).iterator.foldLeft(0) { (max, i) =>
      math.max(max, left(i - 1) + right(i + 1))
    }
  }
}
