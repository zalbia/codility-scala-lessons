package lessons._03.time_complexity._2

// N is an integer within the range [0..100,000];
// the elements of A are all distinct;
// each element of array A is an integer within the range [1..(N + 1)].
object Solution {
  // concise, fast, no allocations
  // requires knowledge of gaussian sum...
  def solution(arr: Array[Int]): Int = // 100% | time: O(n) | space: O(1)
    ((arr.length + 1L) * (arr.length + 2) / 2 - arr.sum).toInt
}

object Alternative {
  // allocates a set of n elements
  def solution(arr: Array[Int]): Int = // 100% | O(n * "eC")
    arr.foldLeft((1 to arr.length + 1).toSet)(_ - _).head
}
