package lessons._06.sorting._2

// N is an integer within the range [3..100,000];
// each element of array A is an integer within the range [−1,000..1,000].
object Solution {
  // the trick is to get only the 3 smallest & 3 biggest subsets
  def solution(a: Array[Int]): Int = { // 100% | time: O(n log n) | space: O(n)
    val subset =
      if (a.length < 6) a
      else {
        val sorted = a.sorted
        sorted.take(3) ++ sorted.takeRight(3)
      }
    subset.combinations(3).map(_.product).max
  }
}
