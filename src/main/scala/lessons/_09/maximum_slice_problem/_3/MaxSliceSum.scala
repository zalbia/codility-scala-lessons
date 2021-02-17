package lessons._09.maximum_slice_problem._3

object Solution {
  // for each element, track max slice sum then update the global max sum
  def solution(a: Array[Int]): Int = { // 100% | T: O(n) | S: O(n)
    val (maxSlice, _) =
      a.foldLeft((Int.MinValue, Int.MinValue)) { case ((maxSlice, currentSum), elem) =>
        val newSum      = if (currentSum < 0) elem else currentSum + elem
        val newMaxSlice = math.max(maxSlice, newSum)
        (newMaxSlice, newSum)
      }
    maxSlice
  }
}
