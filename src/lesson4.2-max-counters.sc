object Solution {
  /**
   * Assumptions:
   * - N and M are integers within the range [1..100,000];
   * - M = a.size
   * - each element of array A is an integer within the range [1..N + 1]
   */
  def solution(n: Int, as: Array[Int]): Array[Int] = { // 88%
    var maxCount = 0
    var counters = Array.fill(n)(0)
    as.foreach { a => // A[K]
      if (a <= n)  {
        val count = counters(a - 1) + 1
        if (count > maxCount) maxCount = count
        counters(a - 1) = count
      }
      if (a == n + 1) counters = Array.fill(n)(maxCount)
    }
    counters
  }
}

Solution.solution(5, Array(3,4,4,6,1,4,4))