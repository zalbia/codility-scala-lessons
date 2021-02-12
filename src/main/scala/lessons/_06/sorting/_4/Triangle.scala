package lessons._06.sorting._4

// N is an integer within the range [0..100,000];
// each element of array A fits in Int.
object Solution {
  // HOFs make this much easier
  def solution(a: Array[Int]): Int = { // 100% | time: O(n log n) | space: O(n)
    def isTriangular(trio: Seq[Long]) = { // Long since addition can overflow
      val (a, b, c) = (trio(0), trio(1), trio(2))
      a + b > c && b + c > a && c + a > b
    }
    if (a.length < 3) 0
    else if (a.sorted.iterator.map(_.toLong).sliding(3).exists(isTriangular)) 1 else 0
  }
}
