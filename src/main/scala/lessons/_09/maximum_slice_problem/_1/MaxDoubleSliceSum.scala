package lessons._09.maximum_slice_problem._1

object Solution {
  def solution(a: Array[Int]): Int = {
    val n = a.length
    val left = a.slice(1, n).scanLeft(0)((b, a) => math.max(0, b + a))
    val right = a.slice(0, n - 1).scanRight(0)((a, b) => math.max(0, a + b))
    (1 until n - 1).iterator.foldLeft(0) { (max, i) =>
      math.max(max, left(i - 1) + right(i + 1))
    }
  }
}
